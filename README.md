
# crc-turbo - CRC generator for ruby

This is a C extention for "crc" gem library.

Just install this, and to do "require 'crc'" only. Additional other work is not required.


## Summary

  * package name: crc-turbo
  * author: dearblue (mailto:dearblue@users.osdn.me)
  * report issue to: <https://osdn.jp/projects/rutsubo/ticket/>
  * how to install: ``gem install crc-turbo``
  * version: 0.2
  * release quality: thechnical preview
  * licensing: BSD-2-Clause<br>any parts are under Creative Commons License Zero (CC0 / Public Domain).
  * dependency gems: crc-0.2 (<https://rubygems/gems/crc>)
  * dependency external c libraries: none
  * bundled external c libraries: none


## How to usage

First, install on your system.

``` shell:shell
# gem install crc-turbo
.....
#
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
