defmodule Mix.Tasks.Compile.MakeBindings do
  def run(_) do
    {_, exit_code} = System.cmd("make", [], into: IO.stream(:stdio, :line))

    case exit_code do
      0 -> :ok
      _ -> :error
    end
  end
end

defmodule Libbtc_ex.Mixfile do
  use Mix.Project

  def project do
    [
      app: :libbtc_ex,
      version: "0.1.10",
      language: :erlang,
      description: "Erlang NIF bindings for the the libbtc library",
      package: [
        files: [
          "Makefile",
          "README.md",
          "c_src/build_deps.sh",
          "c_src/libbtc_nif.c",
          "mix.exs",
          "src/libbtc.erl"
        ],
      ],
      compilers: [:make_bindings, :erlang, :app],
      deps: deps()
    ]
  end

  defp deps() do
    [
      {:mix_erlang_tasks, "0.1.0", runtime: false},
      {:ex_doc, "~> 0.17", only: :dev, runtime: false}
    ]
  end
end
