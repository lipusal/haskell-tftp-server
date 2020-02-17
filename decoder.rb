#!/usr/bin/ruby

if ARGV.empty? do
    puts "Target file required, aborting"
    exit 1
    end
end

MIN_VALUE = 0x20
MAX_VALUE = 0x7F

header, content = ARGF.readlines
key = header.split(/Caesar key: ?/)[1].chomp.to_i
puts "Key: #{key}"
content.each_char { |char| print ((char.ord - key) % MAX_VALUE).chr }
