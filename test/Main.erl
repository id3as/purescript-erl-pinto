-module(test_main@foreign).
-export([filterSasl/0]).

filterSasl() -> fun () ->
  Level = case os:getenv("LOG") of
            false -> none;
            Val -> list_to_atom(Val)
          end,
  logger:set_primary_config(level, Level),
  logger:set_handler_config(default, formatter, {logger_formatter, #{legacy_header => false, single_line => true, depth => 20}}),
  logger:add_primary_filter(sasl, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}})
end.
