use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

pub trait FileSystem {
    fn scan_folder(&mut self, folder: &str) -> HashMap<String, String>;
}

// Used by compiler, reads from files on filesystem
pub struct LocalFS {
    base_path: PathBuf,
    std_path: PathBuf,
}

impl LocalFS {
    pub fn new(base_path: &str) -> Self {
        let std_path = find_std_path(&base_path);
        Self {
            base_path: Path::new(base_path).to_path_buf(),
            std_path,
        }
    }
}

impl FileSystem for LocalFS {
    fn scan_folder(&mut self, folder: &str) -> HashMap<String, String> {
        // TODO asdf This is nasty but it's a quick hack to allow modules from the stdlib to get loaded properly. Ideally LocalFS would have a list of folders to look into to find modules and would know that std/ is one of them.
        let path = if folder == "*current*" {
            self.base_path.clone()
        } else {
            self.std_path.join(folder)
        };

        let files = std::fs::read_dir(path).unwrap();

        let mut ret = HashMap::new();

        for entry in files {
            // This is peak Rust.
            let path = entry
                .unwrap()
                .path()
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();

            if path.ends_with(".brg") {
                let source = std::fs::read_to_string(&path).unwrap();
                let name = std::path::Path::new(&path)
                    .file_name()
                    .unwrap()
                    .to_os_string()
                    .into_string()
                    .unwrap();

                ret.insert(name, source);
            }
        }

        ret
    }
}

fn find_std_path(base: &str) -> PathBuf {
    // Look for `std/` folder in current path
    // Or in compiler base path
    let path = Path::new(base).join("std");

    if path.is_dir() {
        return path;
    }

    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std")
}

// Used by wasm lib, reads from sources passed in from js as json
pub struct WasmFS {
    packages: HashMap<String, HashMap<String, String>>,
}

impl WasmFS {
    pub fn new(packages: HashMap<String, HashMap<String, String>>) -> Self {
        Self { packages }
    }
}

impl FileSystem for WasmFS {
    fn scan_folder(&mut self, folder: &str) -> HashMap<String, String> {
        self.packages
            .get(folder)
            .expect("Module should exist")
            .clone()
    }
}

// Used by tests, doesn't return any files
pub struct NoopFS {}

impl FileSystem for NoopFS {
    fn scan_folder(&mut self, _folder: &str) -> HashMap<String, String> {
        Default::default()
    }
}
