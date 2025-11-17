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
      deps: deps()
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
end
