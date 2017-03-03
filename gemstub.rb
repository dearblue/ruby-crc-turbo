GEMSTUB = Gem::Specification.new do |s|
  s.name = "crc-turbo"
  s.version = "0.3"
  s.summary = "general CRC generator"
  s.description = <<EOS
This is a C extention for "crc" gem library.
Just install this, and to do "require 'crc'" only. Additional other work is not required.
EOS
  s.homepage = "https://github.com/dearblue/ruby-crc-turbo"
  s.licenses = ["BSD-2-Clause", "CC0-1.0"]
  s.author = "dearblue"
  s.email = "dearblue@users.noreply.github.com"

  s.required_ruby_version = ">= 2.0"
  s.add_development_dependency "rake"
  s.add_runtime_dependency "crc", "~> 0.3"
end

EXTMAP["crc"] = "crc/_turbo"
