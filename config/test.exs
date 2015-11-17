use Mix.Config

config :ssdb,
  host: '127.0.0.1',
  port: 6380,
  pool_size: 5,
  password: nil,
  is_reconnect: true,
  proxy_pool_key: 'proxy_pool_test'
