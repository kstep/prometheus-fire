use prometheus_fire::{HistogramVec, IntCounter, IntCounterVec, Metrics};

pub struct Pubkey(String);

#[derive(Metrics)]
#[metric(global = "METRICS", getter = "metrics", subsystem = "notifier", namespace = "global", labels(event_type = "receive", "channel" = "test"))]
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

    println!("{data}");

    assert_eq!(
        data,
        r###"
# HELP global_notifier_listener_reconnects_qty Quantity of listener service reconnects
# TYPE global_notifier_listener_reconnects_qty counter
global_notifier_listener_reconnects_qty 1
# HELP global_notifier_push_qty Quantity of all pushes
# TYPE global_notifier_push_qty counter
global_notifier_push_qty{channel="test",client_id="abc",device_type="android",event_type="receive",service="fcm"} 1
# HELP global_notifier_push_time Time of push processing
# TYPE global_notifier_push_time histogram
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="0.005"} 0
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="0.01"} 0
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="0.025"} 0
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="0.05"} 0
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="0.1"} 0
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="0.25"} 0
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="0.5"} 1
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="1"} 2
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="2.5"} 2
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="5"} 3
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="10"} 3
global_notifier_push_time_bucket{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn",le="+Inf"} 4
global_notifier_push_time_sum{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn"} 106.5
global_notifier_push_time_count{channel="test",client_id="def",device_type="ios",event_type="receive",service="apn"} 4
"###
        .trim_start(),
    );
}
