use Sys.Env

pub def main(): Unit \ {Env, IO} =
    match Env.getArgs() {
        case path :: Nil => mainRunPath(path)
        case _ => println("usage: flix src/*.flix -- <file.jbini>")
    }

def mainRunPath(path: String): Unit \ IO =
    match mainReadSource(path) {
        case Err(_) => println("could not read ${path}")
        case Ok(source) =>
            match mainReadLibraryImports() {
                case Err(msg) => println(msg)
                case Ok(imports) =>
                    let typed = checkWithImports(source, imports);
                    if (typed == "type ok") println(evaluateWithImports(source, imports)) else println(typed)
            }
    }

def mainReadLibraryImports(): Result[String, List[(String, String)]] \ IO =
    mainReadImportFiles(mainLibraryImports(), Nil)

def mainReadImportFiles(files: List[(String, String)], acc: List[(String, String)]): Result[String, List[(String, String)]] \ IO =
    match files {
        case Nil => Ok(acc)
        case (path, importName) :: rest =>
            match mainReadSource(path) {
                case Err(_) => Err("could not read ${path}")
                case Ok(source) => mainReadImportFiles(rest, (importName, source) :: acc)
            }
    }

def mainReadSource(path: String): Result[IoError, String] \ IO =
    Fs.FileRead.runWithIO(() -> Fs.FileRead.read(path))

def mainLibraryImports(): List[(String, String)] =
    ("library/prelude.jbini", "prelude.jbini") ::
    ("library/algebra.jbini", "algebra.jbini") ::
    ("library/combinator.jbini", "combinator.jbini") ::
    ("library/constant.jbini", "constant.jbini") ::
    ("library/natural.jbini", "natural.jbini") ::
    ("library/list.jbini", "list.jbini") ::
    ("library/monad.jbini", "monad.jbini") ::
    ("library/option.jbini", "option.jbini") ::
    ("library/complex.jbini", "complex.jbini") ::
    Nil
