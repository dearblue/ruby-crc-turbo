#!ruby

require "mkmf"

have_type("uint32_t", "stdint.h") or abort
have_type("uint64_t", "stdint.h") or abort
#(have_type("uint128_t") || have_type("uint128_t", "stdint.h")) || have_type("__uint128_t", "stdint.h")

create_makefile File.join("crc", RUBY_VERSION[/\d+\.\d+/], "_turbo")
