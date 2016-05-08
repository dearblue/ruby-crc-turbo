
# crc-turbo - CRC generator for ruby

This is a C extention for "crc" gem library.

Just install this, and "require 'crc'". Additional other work is not required.


## SUMMARY

  * package name: crc-turbo
  * author: dearblue (mailto:dearblue@users.osdn.me)
  * report issue to: <https://osdn.jp/projects/rutsubo/ticket/>
  * how to install: ``gem install crc-turbo``
  * version: 0.1
  * release quality: thechnical preview
  * licensing: BSD-2-Clause
  * dependency gems: crc (<https://rubygems/gems/crc>)
  * dependency external c libraries: none
  * bundled external c libraries: none


## HOW TO USAGE

First, install on your system.

``` shell:shell
# gem install crc-turbo
.....
#
```

And, to do ``require "crc"``.

``` shell:shell
$ ruby -r crc -e 'puts $".grep(/crc/)'
/usr/local/lib/ruby/gems/2.3/gems/crc-turbo-0.1/lib/crc/_turbo.so
/usr/local/lib/ruby/gems/2.3/gems/crc-0.1/lib/crc/_modules.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.1/lib/crc.rb
$
```

If you want not use crc-turbo, set ``RUBY_CRC_NOFAST`` enviroment variable.

``` shell:shell
$ RUBY_CRC_NOFAST=1
$ ruby -r crc -e 'puts $".grep(/crc/)'
/usr/local/lib/ruby/gems/2.3/gems/crc-0.1/lib/crc/_byruby.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.1/lib/crc/_modules.rb
/usr/local/lib/ruby/gems/2.3/gems/crc-0.1/lib/crc.rb
$
```
