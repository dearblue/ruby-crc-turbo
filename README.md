
# crc-turbo - C written accelerator for ruby CRC calcurator

C written accelerator for "crc" gem library.

Just install this, and to do ``require "crc"`` only.

Additional other work is not required.


## How to usage

First, install on your system.

``` shell:shell
# gem install crc-turbo
```

And, to do ``require "crc"``.

``` shell:shell
$ ruby -r crc -e 'puts $".grep(/crc/)'
/usr/local/lib/ruby/gems/2.3/gems/crc-turbo-0.2/lib/crc/2.3/_turbo.so
/usr/local/lib/ruby/gems/2.3/gems/crc-0.2/lib/crc/_modules.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.2/lib/crc/_combine.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.2/lib/crc.rb
$
```

If you want to not use crc-turbo, set ``RUBY_CRC_NOFAST`` enviroment variable.

``` shell:shell
$ export RUBY_CRC_NOFAST=1
$ ruby -r crc -e 'puts $".grep(/crc/)'
/usr/local/lib/ruby/gems/2.3/gems/crc-0.2/lib/crc/_byruby.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.2/lib/crc/_modules.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.2/lib/crc/_combine.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.2/lib/crc.rb
$
```


## Specification

  - package name: crc-turbo (<https://github.com/dearblue/ruby-crc-turbo>)
  - author: dearblue (<https://github.com/dearblue>)
  - report issue to: <https://github.com/dearblue/ruby-crc-turbo/issues>
  - how to install: `gem install crc-turbo`
  - version: 0.4
  - production quality: TECHNICAL PREVIEW
  - licensing:
      - ***BSD-2-Clause : MAIN LICENSE***
      - Creative Commons License Zero (CC0 / Public Domain) : `ext/crc/crc_imps.h`
  - dependency gems: crc-0.4 (<https://rubygems/gems/crc>)
  - dependency external C libraries: none
  - bundled external C libraries: none
