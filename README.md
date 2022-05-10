# prometheus-fire

Prometheus helpers to derive metrics constructor and accessor methods from a struct.

```rust
use prometheus_fire::{HistogramVec, IntCounter, IntCounterVec, Metrics};

#[derive(Metrics)]
pub struct Metrics {
    /// Quantity of all pushes
    #[metric(labels(client_id: &Pubkey = &_.to_string(), service: Service[Fcm|Apn], device_type))]
    push_qty: IntCounterVec,

    /// Time of push processing
    #[metric(labels(client_id: &Pubkey = &_.to_string(), service: Service, device_type))]
    push_time: HistogramVec,

    #[metric(desc = "Quantity of listener service reconnects")]
    listener_reconnects_qty: IntCounter,
}
```

Converted to:

```rust
use prometheus_fire::{HistogramVec, IntCounter, IntCounterVec, Metrics};

pub struct Metrics {
    /// Quantity of all pushes
    #[metric(labels(client_id: &Pubkey = &_.0, service: Service[Fcm|Apn], device_type))]
    push_qty: IntCounterVec,

    /// Time of push processing
    #[metric(labels(client_id: &Pubkey = &_.0, service: Service, device_type))]
    push_time: HistogramVec,

    #[metric(desc = "Quantity of listener service reconnects")]
    listener_reconnects_qty: IntCounter,
}

impl Metrics {
    pub fn new() -> Result<Self, Error> {
        Ok(Self {
            push_qty: register_int_counter_vec!(opts!("push_qty", "Quantity of all pushes"), &["client_id", "service", "device_type"])?,
            push_time: register_histogram_vec!(histogram_opts!("push_time", "Time to push processing"), &["client_id", "service", "device_type"])?,
            listener_reconnects_qty: register_int_counter!(opts!("listener_reconnects_qty", "Quantity of listener service reconnects"))?,
        })
    }

    pub fn push_qty(&self, client_id: &Pubkey, service: Service, device_type: impl AsRef<str>) {
        self.push_qty.with_label_values(&client_id.to_string(), service.as_ref(), device_type.as_ref()).inc();
    }

    pub fn start_push_time(&self, client_id: &Pubkey, service: Service, device_type: impl AsRef<str>) -> HistogramTimer {
        self.push_time.with_label_values(&client_id.to_string(), service.as_ref(), device_type.as_ref()).start()
    }

    pub fn observe_push_time(&self, client_id: &Pubkey, service: Service, device_type: impl AsRef<str>, time: f64) {
        self.push_time.with_label_values(&client_id.to_string(), service.as_ref(), device_type.as_ref()).observe(time);
    }

    pub fn listener_reconnects_qty(&self) {
        self.listener_reconnects_qty.inc();
    }
}

lazy_static! {
    pub static ref METRICS: Metrics = Metrics::new().expect("Can't create a metrics");
}

pub fn metrics() -> &'static Metrics {
    &*METRICS
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Service {
    Fcm,
    Apn,
}

impl AsRef<str> for Service {
    fn as_ref(&self) -> &str {
        match self {
            Self::Fcm => "fcm",
            Self::Apn => "apn",
        }
    }
}
```
