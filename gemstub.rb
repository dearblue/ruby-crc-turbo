GEMSTUB = Gem::Specification.new do |s|
  s.name = "crc-turbo"
  s.version = "0.4"
  s.summary = "C written accelerator for \"crc\" gem library"
  s.description = <<EOS
C written accelerator for "crc" gem library.
Just install this, and to do ``require "crc"`` only.
Additional other work is not required.
EOS
  s.homepage = "https://github.com/dearblue/ruby-crc-turbo"
  s.licenses = ["BSD-2-Clause", "CC0-1.0"]
  s.author = "dearblue"
  s.email = "dearblue@users.noreply.github.com"

  s.required_ruby_version = ">= 2.2"
  s.add_development_dependency "rake"
  s.add_runtime_dependency "crc", "~> 0.4.A"
end

EXTMAP["crc"] = "crc/_turbo"
