GEMSTUB = Gem::Specification.new do |s|
  s.name = "crc-turbo"
  s.version = "0.1"
  s.summary = "general CRC generator"
  s.description = <<EOS
This is a C extention for "crc" gem library.
Just install this, and "require 'crc'". Additional other work is not required.
EOS
  s.homepage = "https://osdn.jp/projects/rutsubo/"
  s.license = "BSD-2-Clause"
  s.author = "dearblue"
  s.email = "dearblue@users.osdn.me"

  s.required_ruby_version = ">= 2.0"
  s.add_development_dependency "rake", "~> 11.0"
  s.add_runtime_dependency "crc", "~> 0.1"
end

EXTMAP["crc"] = "crc/_turbo"
