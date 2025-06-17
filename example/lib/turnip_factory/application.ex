defmodule TurnipFactory.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: TurnipFactory.Registry}
    ]

    opts = [strategy: :one_for_one, name: TurnipFactory.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
