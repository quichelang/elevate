use std::io;
use std::time::Duration;

use game_core::{InputSource, Key};

#[cfg(target_os = "macos")]
pub struct HostInput {
    inner: platform_macos::MacOsInput,
}

#[cfg(target_os = "macos")]
impl HostInput {
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            inner: platform_macos::MacOsInput::new()?,
        })
    }
}

#[cfg(target_os = "macos")]
impl InputSource for HostInput {
    fn poll_key(&mut self, timeout: Duration) -> io::Result<Option<Key>> {
        self.inner.poll_key(timeout)
    }
}

#[cfg(not(target_os = "macos"))]
pub struct HostInput;

#[cfg(not(target_os = "macos"))]
impl HostInput {
    pub fn new() -> io::Result<Self> {
        Err(io::Error::other(
            "HostInput is currently implemented for macOS only",
        ))
    }
}

#[cfg(not(target_os = "macos"))]
impl InputSource for HostInput {
    fn poll_key(&mut self, _timeout: Duration) -> io::Result<Option<Key>> {
        Err(io::Error::other(
            "HostInput polling is currently implemented for macOS only",
        ))
    }
}

pub fn platform_name() -> &'static str {
    std::env::consts::OS
}
