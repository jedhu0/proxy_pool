# ProxyPool

**Warning**
- This tool should work with ssdb
- Ensure that you had save your proxys in ssdb

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed as:

  1. Add proxy_pool to your list of dependencies in `mix.exs`:

        def deps do
          [{:proxy_pool, "~> 0.0.1"}]
        end

  2. Ensure proxy_pool is started before your application:

        def application do
          [applications: [:proxy_pool]]
        end

  3. Ensure you had config like this in your project:
  ```elixir
    config :proxy_pool,
      ssdb_key: 'xxx_proxy_pool', # the key with proxy in ssdb, use char list
      test_source: %{baidu: "www.baidu.com", google: "www.google.com"} # the source host for test
  ```

  4. APIs
  ```elixir
    # get random proxy from pool
    # you can use the source in config
    iex(1)> ProxyPool.random(source)
    "127.0.0.1:8086"

    # call the server if the proxy was invalid
    iex(2)> p = ProxyPool.random(source)
    "127.0.0.1:8087"
    iex(3)> ProxyPool.fail_notice(source, p)
    :ok

    # udpate the state with proxy_pool server with your ssdb updated
    iex(4)> ProxyPool.update
    :ok
  ```
