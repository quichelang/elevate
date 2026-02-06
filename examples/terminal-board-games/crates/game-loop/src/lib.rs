use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StepPlan {
    pub updates: u32,
    pub interpolation: f32,
}

#[derive(Debug, Clone)]
pub struct FixedStepLoop {
    step: Duration,
    max_updates_per_frame: u32,
    accumulator: Duration,
}

impl FixedStepLoop {
    pub fn new(step: Duration) -> Self {
        Self {
            step,
            max_updates_per_frame: 8,
            accumulator: Duration::ZERO,
        }
    }

    pub fn with_max_updates_per_frame(mut self, max_updates_per_frame: u32) -> Self {
        self.max_updates_per_frame = max_updates_per_frame.max(1);
        self
    }

    pub fn step(&self) -> Duration {
        self.step
    }

    pub fn plan_for_elapsed(&mut self, elapsed: Duration) -> StepPlan {
        self.accumulator = self.accumulator.saturating_add(elapsed);

        let step_nanos = self.step.as_nanos();
        if step_nanos == 0 {
            return StepPlan {
                updates: 1,
                interpolation: 0.0,
            };
        }

        let mut updates = (self.accumulator.as_nanos() / step_nanos) as u32;
        if updates > self.max_updates_per_frame {
            updates = self.max_updates_per_frame;
        }

        if updates > 0 {
            let consumed = Duration::from_nanos((step_nanos * u128::from(updates)) as u64);
            self.accumulator = self.accumulator.saturating_sub(consumed);
        }

        let interpolation = (self.accumulator.as_nanos() as f64 / step_nanos as f64) as f32;
        StepPlan {
            updates,
            interpolation: interpolation.clamp(0.0, 1.0),
        }
    }

    pub fn reset(&mut self) {
        self.accumulator = Duration::ZERO;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_elapsed_time_gives_no_updates() {
        let mut loop_ctrl = FixedStepLoop::new(Duration::from_millis(16));
        let plan = loop_ctrl.plan_for_elapsed(Duration::ZERO);
        assert_eq!(plan.updates, 0);
        assert_eq!(plan.interpolation, 0.0);
    }

    #[test]
    fn elapsed_time_produces_updates_and_interpolation() {
        let mut loop_ctrl = FixedStepLoop::new(Duration::from_millis(16));
        let plan = loop_ctrl.plan_for_elapsed(Duration::from_millis(20));
        assert_eq!(plan.updates, 1);
        assert!(plan.interpolation > 0.0);
        assert!(plan.interpolation < 1.0);
    }

    #[test]
    fn update_cap_is_enforced() {
        let mut loop_ctrl =
            FixedStepLoop::new(Duration::from_millis(10)).with_max_updates_per_frame(3);
        let plan = loop_ctrl.plan_for_elapsed(Duration::from_millis(100));
        assert_eq!(plan.updates, 3);
    }
}
