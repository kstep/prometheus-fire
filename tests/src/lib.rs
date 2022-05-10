use prometheus_fire::{HistogramVec, IntCounter, IntCounterVec, Metrics};

pub struct Pubkey(String);

#[derive(Metrics)]
pub struct Metrics {
    /// Quantity of all pushes
    #[metric(labels(client_id: &Pubkey = &_.0, service: Service[Fcm|Apn], device_type))]
    push_qty: IntCounterVec,

    /// Time of push processing
    #[metric(labels(client_id: &Pubkey = &_.0, service: Service[Fcm|Apn], device_type))]
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
    metrics.observe_push_time(&Pubkey("def".into()), Service::Apn, "ios", 0.5);
    metrics.observe_push_time(&Pubkey("def".into()), Service::Apn, "ios", 1.0);
    metrics.observe_push_time(&Pubkey("def".into()), Service::Apn, "ios", 5.0);
    metrics.observe_push_time(&Pubkey("def".into()), Service::Apn, "ios", 100.0);

    let data = Metrics::gather().expect("metrics gathered");

    assert_eq!(
        data,
        r###"
# HELP listener_reconnects_qty Quantity of listener service reconnects
# TYPE listener_reconnects_qty counter
listener_reconnects_qty 1
# HELP push_qty Quantity of all pushes
# TYPE push_qty counter
push_qty{client_id="abc",device_type="android",service="fcm"} 1
# HELP push_time Time of push processing
# TYPE push_time histogram
push_time_bucket{client_id="def",device_type="ios",service="apn",le="0.005"} 0
push_time_bucket{client_id="def",device_type="ios",service="apn",le="0.01"} 0
push_time_bucket{client_id="def",device_type="ios",service="apn",le="0.025"} 0
push_time_bucket{client_id="def",device_type="ios",service="apn",le="0.05"} 0
push_time_bucket{client_id="def",device_type="ios",service="apn",le="0.1"} 0
push_time_bucket{client_id="def",device_type="ios",service="apn",le="0.25"} 0
push_time_bucket{client_id="def",device_type="ios",service="apn",le="0.5"} 1
push_time_bucket{client_id="def",device_type="ios",service="apn",le="1"} 2
push_time_bucket{client_id="def",device_type="ios",service="apn",le="2.5"} 2
push_time_bucket{client_id="def",device_type="ios",service="apn",le="5"} 3
push_time_bucket{client_id="def",device_type="ios",service="apn",le="10"} 3
push_time_bucket{client_id="def",device_type="ios",service="apn",le="+Inf"} 4
push_time_sum{client_id="def",device_type="ios",service="apn"} 106.5
push_time_count{client_id="def",device_type="ios",service="apn"} 4
"###
        .trim_start(),
    );
}
