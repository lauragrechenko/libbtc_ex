ExUnit.configure(exclude: [pending: true])

ExUnit.start()
Application.ensure_all_started(:libbtc_ex)

