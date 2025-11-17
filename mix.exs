defmodule DdtraceUmbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      aliases: aliases()
    ]
  end

  defp aliases do
    [
      "escript.build": ["do --app model escript.build"]
    ]
  end
end
