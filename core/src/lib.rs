use hyper::{
    service::{make_service_fn, service_fn},
    Body, Method, Request, Response, Server, StatusCode,
};
use prometheus::{Error, TextEncoder};
use std::{convert::Infallible, future::Future, net::SocketAddr, pin::Pin};

pub use prometheus::{HistogramVec, IntCounter, IntCounterVec};
#[cfg(feature = "derive")]
pub use prometheus_fire_derive::Metrics;

pub trait MetricsService {
    fn gather() -> Result<String, Error> {
        let metric_families = prometheus::gather();
        TextEncoder::new().encode_to_string(&metric_families)
    }

    fn serve(&self, bind: impl Into<SocketAddr>) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        let addr = bind.into();
        log::info!("running metrics endpoint at http://{addr}...");

        let new_service = make_service_fn(|_| async {
            Ok::<_, Infallible>(service_fn(|req: Request<Body>| async move {
                log::info!("request {} {}", req.method(), req.uri().path());
                if req.method() != Method::GET || req.uri().path() != "/" {
                    return Ok::<_, Infallible>(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(Body::empty())
                            .unwrap(),
                    );
                }

                let metrics = Self::gather().unwrap();
                let reply = Response::builder()
                    .header("Content-Type", "text/plain; version=0.0.4")
                    .body(Body::from(metrics))
                    .unwrap();
                Ok(reply)
            }))
        });

        Box::pin(async move {
            if let Err(error) = Server::bind(&addr).serve(new_service).await {
                log::error!("error serving metrics: {error}");
            }
        })
    }
}
