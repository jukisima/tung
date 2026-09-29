class Tung < Formula
  desc "a functional programming tongue with infix notation"
  homepage "https://github.com/jukisima/tung"
  url "https://github.com/jukisima/tung.git",
      revision: "b63b57a1afca95e89a81e37b5e61247d21c5c882"
  version "0.1.0.0"

  head "https://github.com/jukisima/tung.git", branch: "main"

  depends_on "cabal-install" => :build
  depends_on "ghc@9.12" => :build

  def fetch
    ENV.prepend_path "PATH", Formula["ghc@9.12"].opt_bin

    cd "tongue" do
      system "cabal", "v2-update"
      system "cabal", "v2-build", "exe:tung", "--only-download"
    end
  end

  def install
    ENV.prepend_path "PATH", Formula["ghc@9.12"].opt_bin

    cd "tongue" do
      system "cabal", "v2-build", "exe:tung", "--offline", "--jobs=#{ENV.make_jobs}"
      bin.install Utils.safe_popen_read("cabal", "list-bin", "exe:tung").strip
    end
  end

  test do
    source = "let value identity = value\n"
    assert_equal "type ok", pipe_output("#{bin}/tung --check-stdin", source).strip
  end
end
