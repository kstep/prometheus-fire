use prometheus_fire::{HistogramVec, IntCounter, IntCounterVec, Metrics};

pub enum Service {
    Fcm,
    Apn,
}

pub struct Pubkey(String);

impl AsRef<str> for Service {
    fn as_ref(&self) -> &str {
        match self {
            Self::Fcm => "fcm",
            Self::Apn => "apn",
        }
    }
}

#[derive(Metrics)]
pub struct Metrics {
    /// Quantity of all pushes
    #[metric(labels(client_id: &Pubkey = &_.0, service: Service, device_type))]
    push_qty: IntCounterVec,

    /// Time of push processing
    #[metric(labels(client_id: &Pubkey = &_.0, service: Service, device_type))]
    push_time: HistogramVec,

    #[metric(desc = "Quantity of listener service reconnects")]
    listener_reconnects_qty: IntCounter,
}

#[test]
fn test_metrics() {
    use prometheus_fire::MetricsService;
    let metrics = metrics();
    metrics.listener_reconnects_qty();
    metrics.push_qty(&Pubkey("abc".into()), Service::Fcm, "android");

    let data = Metrics::gather().expect("metrics gathered");

    println!("{}", data);
}
