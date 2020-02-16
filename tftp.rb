#!/usr/bin/ruby

# Depends on `net-tftp` gem, see https://rubygems.org/gems/net-tftp

require 'net/tftp'

host = "lipusal.redirectme.net"
port = 7000
puts "Connectint to #{host}:#{port}..."

conn = Net::TFTP.new host, port: port

puts "About to get file"
conn.getbinaryfile("tftp.txt")
puts "Got file"
