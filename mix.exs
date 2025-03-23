defmodule Dlstalk do
  use Mix.Project

  def project do
    [
      app: :dlstalk,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      compilers: [:erlang] ++ Mix.compilers(),
      deps: [],
      escript: [
        main_module: Dlstalk.Main,
        emu_args: "-sname dlstalk +P 10485760"
      ]
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end
end
