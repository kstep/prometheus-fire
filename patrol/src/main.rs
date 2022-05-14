use futures_util::{stream, FutureExt, Stream, StreamExt, TryFutureExt, TryStreamExt};
use hyper::{
    body::Bytes,
    client::HttpConnector,
    service::{make_service_fn, service_fn},
    Body, Client, Request, Response, Server, StatusCode, Uri,
};
use hyper_tls::HttpsConnector;
use serde::Deserialize;
use serde_with::{serde_as, DisplayFromStr};
use std::{convert::Infallible, fmt::Write, future::ready, net::SocketAddr, sync::Arc};

static APP_ENV_PREFIX: &str = "FIRE_PATROL_";
static PARALLELISM: usize = 4;

type HttpsClient = Client<HttpsConnector<HttpConnector>, Body>;
type BoxError = Box<dyn std::error::Error + Send + Sync + 'static>;

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("hyper error: {0}")]
    Hyper(#[from] hyper::Error),
    #[error("http error: {0:?}")]
    Http(StatusCode),
}

#[serde_as]
#[derive(Debug, Deserialize)]
struct Config {
    #[serde_as(as = "Vec<DisplayFromStr>")]
    services: Vec<Uri>,
    #[serde_as(as = "DisplayFromStr")]
    bind_address: SocketAddr,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();
    env_logger::init();

    let Config { services, bind_address } = envy::prefixed(APP_ENV_PREFIX).from_env()?;

    let client = Arc::new(Client::builder().build(HttpsConnector::new()));
    let services = Arc::new(services);

    log::info!("running server at http://{bind_address}...");
    let server = Server::bind(&bind_address).serve(make_service_fn(move |_| {
        let client = client.clone();
        let services = services.clone();

        async move {
            Ok::<_, Infallible>(service_fn(move |req: Request<Body>| {
                log::info!("request: {} {}", req.method(), req.uri());
                let client = client.clone();
                let services = services.clone();

                let status = run_service_patrol(client.clone(), services.clone())
                    .map(|(url, status)| Bytes::from(format_status(&url, &status)))
                    .map(Ok::<_, BoxError>);

                let body =
                    Body::from(Box::new(status) as Box<dyn Stream<Item = Result<Bytes, BoxError>> + Send + 'static>);

                ready(
                    Response::builder()
                        .header("Content-Type", "text/plain; version=0.4.0")
                        .body(body),
                )
            }))
        }
    }));

    server.await?;

    Ok(())
}

fn run_service_patrol(
    client: Arc<HttpsClient>,
    services: Arc<Vec<Uri>>,
) -> impl Stream<Item = (String, Result<String, Error>)> {
    let services = Vec::clone(&services);
    stream::iter(services.into_iter().map(move |uri| {
        let url = uri.to_string();
        client
            .get(uri)
            .err_into::<Error>()
            .and_then(http_error)
            .and_then(read_body)
            .map(|result| (url, result))
    }))
    .buffer_unordered(PARALLELISM)
}

fn bytes_to_str(bytes: Bytes) -> String {
    String::from_utf8_lossy(bytes.as_ref()).into_owned()
}

async fn http_error(response: Response<Body>) -> Result<Response<Body>, Error> {
    if response.status() == StatusCode::OK {
        Ok(response)
    } else {
        Err(Error::Http(response.status()))
    }
}

async fn read_body(response: Response<Body>) -> Result<String, Error> {
    response
        .into_body()
        .err_into::<Error>()
        .map_ok(bytes_to_str)
        .try_collect::<String>()
        .await
}

fn format_status(url: &str, status: &Result<String, Error>) -> String {
    let mut buf = String::new();
    buf.write_fmt(format_args!("# Server {url} status\n")).ok();
    match status {
        Ok(metrics) => {
            if metrics.is_empty() {
                buf.write_str("# Empty metrics\n").ok();
            } else {
                buf.write_str(metrics).ok();
            }
        },
        Err(error) => {
            buf.write_fmt(format_args!("# ERROR: {error}\n")).ok();
        },
    }
    buf
}
