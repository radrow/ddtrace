defmodule MicrochipFactory.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: MicrochipFactory.Registry}
    ]

    opts = [strategy: :one_for_one, name: MicrochipFactory.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
