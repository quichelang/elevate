use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;

#[derive(Debug)]
pub enum SaveError {
    Io(io::Error),
    Parse(String),
}

impl fmt::Display for SaveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SaveError::Io(err) => write!(f, "io error: {err}"),
            SaveError::Parse(msg) => write!(f, "parse error: {msg}"),
        }
    }
}

impl std::error::Error for SaveError {}

impl From<io::Error> for SaveError {
    fn from(value: io::Error) -> Self {
        SaveError::Io(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Snapshot {
    pub kind: String,
    pub version: u16,
    pub fields: BTreeMap<String, String>,
}

impl Snapshot {
    pub fn new(kind: impl Into<String>, version: u16) -> Self {
        Self {
            kind: kind.into(),
            version,
            fields: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.fields.insert(key.into(), value.into());
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.fields.get(key).map(String::as_str)
    }

    pub fn encode(&self) -> String {
        let mut out = String::new();
        out.push_str("kind=");
        out.push_str(&escape(&self.kind));
        out.push('\n');

        out.push_str("version=");
        out.push_str(&self.version.to_string());
        out.push('\n');

        for (key, value) in &self.fields {
            out.push_str("field.");
            out.push_str(&escape(key));
            out.push('=');
            out.push_str(&escape(value));
            out.push('\n');
        }

        out
    }

    pub fn decode(content: &str) -> Result<Self, SaveError> {
        let mut kind = None;
        let mut version = None;
        let mut fields = BTreeMap::new();

        for line in content.lines() {
            if line.trim().is_empty() {
                continue;
            }

            let (key, value) = line
                .split_once('=')
                .ok_or_else(|| SaveError::Parse(format!("missing '=' in line '{line}'")))?;

            if key == "kind" {
                kind = Some(unescape(value)?);
                continue;
            }

            if key == "version" {
                let parsed = value
                    .parse::<u16>()
                    .map_err(|_| SaveError::Parse(format!("invalid version number '{value}'")))?;
                version = Some(parsed);
                continue;
            }

            if let Some(encoded_field) = key.strip_prefix("field.") {
                fields.insert(unescape(encoded_field)?, unescape(value)?);
                continue;
            }

            return Err(SaveError::Parse(format!("unknown key '{key}'")));
        }

        Ok(Self {
            kind: kind.ok_or_else(|| SaveError::Parse("missing kind".to_string()))?,
            version: version.ok_or_else(|| SaveError::Parse("missing version".to_string()))?,
            fields,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReplayEvent {
    pub frame: u64,
    pub key: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ReplayLog {
    pub events: Vec<ReplayEvent>,
}

impl ReplayLog {
    pub fn encode(&self) -> String {
        let mut out = String::new();
        out.push_str("version=1\n");
        for event in &self.events {
            out.push_str("event=");
            out.push_str(&event.frame.to_string());
            out.push(':');
            out.push_str(&escape(&event.key));
            out.push('\n');
        }
        out
    }

    pub fn decode(content: &str) -> Result<Self, SaveError> {
        let mut events = Vec::new();

        for line in content.lines() {
            if line.trim().is_empty() {
                continue;
            }

            if line.starts_with("version=") {
                continue;
            }

            let payload = line
                .strip_prefix("event=")
                .ok_or_else(|| SaveError::Parse(format!("invalid replay line '{line}'")))?;
            let (frame, key) = payload
                .split_once(':')
                .ok_or_else(|| SaveError::Parse(format!("invalid event payload '{payload}'")))?;
            let frame = frame
                .parse::<u64>()
                .map_err(|_| SaveError::Parse(format!("invalid frame '{frame}'")))?;
            events.push(ReplayEvent {
                frame,
                key: unescape(key)?,
            });
        }

        Ok(Self { events })
    }
}

pub fn write_snapshot(path: impl AsRef<Path>, snapshot: &Snapshot) -> Result<(), SaveError> {
    fs::write(path, snapshot.encode())?;
    Ok(())
}

pub fn read_snapshot(path: impl AsRef<Path>) -> Result<Snapshot, SaveError> {
    let content = fs::read_to_string(path)?;
    Snapshot::decode(&content)
}

pub fn write_replay(path: impl AsRef<Path>, replay: &ReplayLog) -> Result<(), SaveError> {
    fs::write(path, replay.encode())?;
    Ok(())
}

pub fn read_replay(path: impl AsRef<Path>) -> Result<ReplayLog, SaveError> {
    let content = fs::read_to_string(path)?;
    ReplayLog::decode(&content)
}

fn escape(value: &str) -> String {
    let mut out = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '=' => out.push_str("\\="),
            ':' => out.push_str("\\:"),
            _ => out.push(ch),
        }
    }
    out
}

fn unescape(value: &str) -> Result<String, SaveError> {
    let mut out = String::new();
    let mut chars = value.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }

        let next = chars
            .next()
            .ok_or_else(|| SaveError::Parse("dangling escape".to_string()))?;
        let decoded = match next {
            '\\' => '\\',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '=' => '=',
            ':' => ':',
            other => {
                return Err(SaveError::Parse(format!(
                    "unknown escape sequence '\\{other}'"
                )));
            }
        };
        out.push(decoded);
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_roundtrip_keeps_fields() {
        let mut snapshot = Snapshot::new("sudoku", 1);
        snapshot.insert("cells", "123\\n456");
        snapshot.insert("note", "A=B:C");

        let encoded = snapshot.encode();
        let decoded = Snapshot::decode(&encoded).expect("decode");
        assert_eq!(decoded, snapshot);
    }

    #[test]
    fn replay_roundtrip_keeps_events() {
        let replay = ReplayLog {
            events: vec![
                ReplayEvent {
                    frame: 12,
                    key: "Left".to_string(),
                },
                ReplayEvent {
                    frame: 34,
                    key: "Char(:)".to_string(),
                },
            ],
        };

        let encoded = replay.encode();
        let decoded = ReplayLog::decode(&encoded).expect("decode");
        assert_eq!(decoded, replay);
    }
}
