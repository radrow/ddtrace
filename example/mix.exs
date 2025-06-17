defmodule TurnipFactory.MixProject do
  use Mix.Project

  def project do
    [
      app: :turnip_factory,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      compilers: [:erlang] ++ Mix.compilers(),
      erlc_paths: ["src"],
      erlc_include_path: "src",
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {TurnipFactory.Application, []}
    ]
  end

  defp deps do
    []
  end
end
