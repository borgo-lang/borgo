use std::collections::HashMap;

use crate::ast::{Expr, File, Function, ParseError, Span, UnparsedFile};
use crate::codegen;
use crate::error;
use crate::global_state::Declaration;
use crate::infer;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub files: Vec<File>,
    pub errors: HashMap<String, Vec<error::Error>>,
}

impl Package {
    /// Given some files which may have failed to parse, create a Package.
    /// Errors are initialized with existing parse errors
    pub fn from_files(name: String, files: Vec<(File, Vec<ParseError>)>) -> Self {
        let mut new_errors = HashMap::new();

        let new_files = files
            .into_iter()
            .map(|(file, errors)| {
                let errors = errors.into_iter().map(error::Error::Parse).collect();
                new_errors.insert(file.name.clone(), errors);
                file
            })
            .collect();

        Self {
            name,
            files: new_files,
            errors: new_errors,
        }
    }

    pub fn from_file_contents(name: String, files: Vec<UnparsedFile>) -> Self {
        let files = files
            .into_iter()
            .map(|f| File::from_file_contents(&f.filename, &f.contents))
            .collect();

        Package::from_files(name, files)
    }

    /// Find the first error across all files in a package. Returns (file path, error)
    pub fn first_error(&self) -> Option<(File, error::Error)> {
        self.errors.iter().find_map(|(path, e)| {
            e.first().map(|err| {
                let file = self.find_file_by_path(path).unwrap();
                (file, err.clone())
            })
        })
    }

    fn find_file_by_path(&self, path: &str) -> Option<File> {
        self.files.iter().find(|f| f.name == path).cloned()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Project {
    pub packages: HashMap<String, Package>,
    pub output: HashMap<String, codegen::EmittedFile>,
}

impl Project {
    pub fn new() -> Self {
        Self {
            packages: Default::default(),
            output: Default::default(),
        }
    }

    pub fn from_packages(packages: Vec<Package>) -> Self {
        Self {
            packages: packages
                .into_iter()
                .map(|pkg| (pkg.name.clone(), pkg))
                .collect(),
            output: Default::default(),
        }
    }

    pub fn user_package(&self) -> Option<Package> {
        self.packages.get(&Self::user()).cloned()
    }

    /// Errors are just included in Package, this function doesn't return a Result.
    /// There may be multiple errors in a single Package, and compiling a Project may have multiple
    /// errors from different packages.
    pub fn compile(
        &mut self,
        name: String,
        instance: &mut infer::Infer,
        files: Vec<UnparsedFile>,
    ) -> Package {
        let files = files
            .into_iter()
            .map(|f| File::from_file_contents(&f.filename, &f.contents))
            .collect();

        let pkg = Package::from_files(name.clone(), files);
        let new_pkg = instance.infer_package(&pkg);

        self.packages.insert(name, new_pkg.clone());
        new_pkg
    }

    pub fn std() -> String {
        "std".to_string()
    }
    pub fn user() -> String {
        "user".to_string()
    }

    pub fn to_json(&self) -> ProjectJSON {
        // There's probably a better way of going about this :/
        let std = self.packages.get(&Self::std()).cloned().unwrap();
        let pkg = self.user_package().unwrap();

        let first_error =
            std.first_error()
                .or_else(|| pkg.first_error())
                .map(|err: (File, error::Error)| ErrorJSON {
                    kind: "error".to_string(),
                    filename: err.0.name,
                    msg: err.1.stringify(Some(&err.0.source)),
                    span: err.1.get_span(),
                });

        let packages = vec![PackageJSON(std), PackageJSON(pkg)];

        ProjectJSON {
            packages,
            first_error,
        }
    }

    pub fn sorted_packages(&self) -> Vec<Package> {
        // TODO sort in topological order as in, following dependencies
        vec![
            self.packages.get(&Self::std()).unwrap().clone(),
            self.packages.get(&Self::user()).unwrap().clone(),
        ]
    }

    pub fn infer(&mut self, instance: &mut infer::Infer) {
        self.sorted_packages().into_iter().for_each(|pkg| {
            let new_pkg = instance.infer_package(&pkg);
            self.packages.insert(pkg.name, new_pkg);
        });
    }

    pub fn build(&mut self, instance: &mut codegen::Codegen) {
        self.sorted_packages().into_iter().for_each(|pkg| {
            let emitted = instance.compile_package(&pkg);
            self.output.insert(pkg.name, emitted);
        });
    }

    pub fn find_declaration_at(&self, decl: &Declaration) -> Option<Expr> {
        let file = self
            .packages
            .get(&decl.file_id.package)?
            .find_file_by_path(&decl.file_id.filename)?;

        file.find_expr_at_position(&decl.span)
    }

    // This function assumes the declaration is valid and can be found
    // Walk all the definitions of a var to get to the original declaration
    pub fn find_function_declaration_at(&self, decl: &Declaration) -> Function {
        let expr = self.find_declaration_at(decl).unwrap();
        match expr {
            Expr::Var { decl, .. } => self.find_function_declaration_at(&decl),
            Expr::Closure { fun, .. } => fun,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
/// Wrap Package so that errors can be sorted
/// (used in expectation tests to not modify output on every run)
pub struct PackageJSON(pub Package);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectJSON {
    pub packages: Vec<PackageJSON>,
    pub first_error: Option<ErrorJSON>,
}

impl Serialize for PackageJSON {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;

        let pkg = &self.0;
        let mut errors = Vec::from_iter(pkg.errors.iter());
        errors.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

        let mut state = serializer.serialize_struct("Package", 2)?;
        state.serialize_field("files", &pkg.files)?;
        state.serialize_field("errors", &errors)?;
        state.end()
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ErrorJSON {
    pub kind: String,
    pub filename: String,
    pub msg: String,
    pub span: Span,
}
