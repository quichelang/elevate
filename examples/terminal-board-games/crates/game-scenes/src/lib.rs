use game_core::{Frame, Key};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SceneCommand<S> {
    None,
    Push(S),
    Pop,
    Replace(S),
    Quit,
}

pub trait Scene: Sized {
    fn title(&self) -> &str;
    fn on_key(&mut self, key: Key) -> SceneCommand<Self>;
    fn render(&self, frame: &mut Frame);
}

#[derive(Debug, Clone)]
pub struct SceneStack<S> {
    stack: Vec<S>,
    quit_requested: bool,
}

impl<S> SceneStack<S> {
    pub fn new(root: S) -> Self {
        Self {
            stack: vec![root],
            quit_requested: false,
        }
    }

    pub fn current(&self) -> Option<&S> {
        self.stack.last()
    }

    pub fn current_mut(&mut self) -> Option<&mut S> {
        self.stack.last_mut()
    }

    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    pub fn quit_requested(&self) -> bool {
        self.quit_requested
    }

    pub fn apply(&mut self, command: SceneCommand<S>) {
        match command {
            SceneCommand::None => {}
            SceneCommand::Push(scene) => self.stack.push(scene),
            SceneCommand::Pop => {
                if self.stack.len() > 1 {
                    self.stack.pop();
                } else {
                    self.quit_requested = true;
                }
            }
            SceneCommand::Replace(scene) => {
                if self.stack.is_empty() {
                    self.stack.push(scene);
                } else {
                    let _ = self.stack.pop();
                    self.stack.push(scene);
                }
            }
            SceneCommand::Quit => self.quit_requested = true,
        }
    }
}

impl<S> SceneStack<S>
where
    S: Scene,
{
    pub fn on_key(&mut self, key: Key) {
        let Some(scene) = self.current_mut() else {
            self.quit_requested = true;
            return;
        };
        let command = scene.on_key(key);
        self.apply(command);
    }

    pub fn render_current(&self, frame: &mut Frame) {
        if let Some(scene) = self.current() {
            scene.render(frame);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum TestScene {
        Root,
        Menu,
    }

    impl Scene for TestScene {
        fn title(&self) -> &str {
            match self {
                TestScene::Root => "Root",
                TestScene::Menu => "Menu",
            }
        }

        fn on_key(&mut self, key: Key) -> SceneCommand<Self> {
            match (&*self, key) {
                (TestScene::Root, Key::Char('m')) => SceneCommand::Push(TestScene::Menu),
                (TestScene::Menu, Key::Escape) => SceneCommand::Pop,
                (_, Key::Char('q')) => SceneCommand::Quit,
                _ => SceneCommand::None,
            }
        }

        fn render(&self, _frame: &mut Frame) {}
    }

    #[test]
    fn push_and_pop_scenes() {
        let mut stack = SceneStack::new(TestScene::Root);
        stack.on_key(Key::Char('m'));
        assert_eq!(stack.depth(), 2);

        stack.on_key(Key::Escape);
        assert_eq!(stack.depth(), 1);
        assert!(!stack.quit_requested());
    }

    #[test]
    fn quit_command_sets_flag() {
        let mut stack = SceneStack::new(TestScene::Root);
        stack.on_key(Key::Char('q'));
        assert!(stack.quit_requested());
    }
}
