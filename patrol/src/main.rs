use futures_util::{stream, StreamExt, TryStreamExt};
use hyper::{body::Bytes, client::HttpConnector, Body, Client, Response, StatusCode, Uri};
use hyper_tls::HttpsConnector;
use serde::Deserialize;
use serde_with::{serde_as, DisplayFromStr, DurationSeconds};
use std::time::Duration;
use tokio::time::sleep;

static APP_ENV_PREFIX: &str = "FIRE_PATROL_";
static PARALLELISM: usize = 4;

type HttpsClient = Client<HttpsConnector<HttpConnector>, Body>;

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
    #[serde_as(as = "DurationSeconds")]
    delay: Duration,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();
    env_logger::init();

    let Config { services, delay } = envy::prefixed(APP_ENV_PREFIX).from_env()?;

    let client = Client::builder().build(HttpsConnector::new());

    loop {
        match run_service_patrol(&client, &services).await {
            Ok(statistics) => {
                println!("{statistics}");
            },
            Err(error) => {
                log::error!("service lookup failure: {error}");
            },
        }

        sleep(delay).await;
    }

    Ok(())
}

async fn run_service_patrol(client: &HttpsClient, services: &[Uri]) -> Result<String, Error> {
    let responses = stream::iter(services.iter().cloned().map(|uri| client.get(uri)))
        .buffer_unordered(PARALLELISM)
        .err_into::<Error>()
        .and_then(http_error)
        .inspect(|reply| {
            log::info!("{reply:?}");
        })
        .map_ok(Response::into_body)
        .map_ok(TryStreamExt::err_into::<Error>)
        .try_flatten()
        .map_ok(bytes_to_str)
        .try_collect::<String>()
        .await?;
    Ok(responses)
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
