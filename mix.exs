defmodule Ddmon do
  use Mix.Project

  def project do
    [
      app: :ddmon,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      compilers: [:erlang] ++ Mix.compilers(),
      deps: [],
      escript: [
        main_module: Ddmon.Main,
        emu_args: "-sname ddmon +P 10485760"
      ]
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end
end
