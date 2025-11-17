defmodule Model.MixProject do
  use Mix.Project

  def project do
    [
      app: :model,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      compilers: [:erlang] ++ Mix.compilers(),
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      erlc_paths: ["src"],
      erlc_include_path: "include",
      deps: deps(),
      escript: escript()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:ddtrace, in_umbrella: true}
    ]
  end

  defp escript do
    [
      name: "ddtrace",
      main_module: DDTrace.Main,
      emu_args: "-sname ddtrace +P 10485760",
      path: Path.expand("../../ddtrace", __DIR__)
    ]
  end
end
