#!/usr/bin/env ruby

require 'optparse'

class Array
	def flatten_ranges
		self.map {|r| r.to_a}.flatten
	end
end

srand "benkudria".split('').inject(1) {|a, c| a*c[0]}

@options = {:color => :dark}

@hostnames =
	{
	'laotzu'                          => 34,
	'kudria.net'                      => nil,
	'weyl.ams.sunysb.edu'             => nil,
	'seawulf.stonybrook.edu'          => nil,
	'sparky.ic.sunysb.edu'            => nil,
	'greatbeyond.bsdwebsolutions.com' => nil,
	'galaxy.ams.sunysb.edu'           => nil,
	'olympus.ams.sunysb.edu'          => nil,
}

@nyt_hostnames =
	{
	'app1.prvt.nytimes.com'    => nil,
	'dapi.prvt.nytimes.com'    => nil,
	'cvs.prvt.nytimes.com'     => nil,
	'ddeploy.prvt.nytimes.com' => nil,
	'dmysql1.prvt.nytimes.com' => nil,
	'svn.prvt.nytimes.com'     => nil,
	'dsvn.prvt.nytimes.com'    => nil,
}

@galaxy_hostnames =
	{
	'sirius'      => nil,
	'hortal'      => nil,
	'sagittarius' => nil,
	'starzero'    => nil,
	'centauri01'  => nil,
	'centauri02'  => nil,
	'tauri01'     => nil,
	'tauri02'     => nil,
	'aurigae'     => nil,
	'aquilae'     => nil,
	'crux'        => nil,
}

@seawulf_hostnames =
	{
	'nagling'      => nil,
	'grendel'      => nil,
	'herot'        => nil,
	'wiglaf'       => nil,
	'wulfie'       => nil,
}

@younoodle_hostnames =
	{
	'dev.younoodle.org'     => nil,
    'web3.sf.younoodle.com' => nil,
    'web4.sf.younoodle.com' => nil,
}

@hostnames.merge! @galaxy_hostnames
@hostnames.merge! @seawulf_hostnames
@hostnames.merge! @nyt_hostnames
@hostnames.merge! @younoodle_hostnames

@lightlist = [66..66, 131..131, 143..146].flatten_ranges
@light_colors = ([20..21, 25..51, 56..57,
                  61..87, 90..231].flatten_ranges - @lightlist).reverse.shuffle

@dark_colors = [16..33, 52..71, 88..100,
                124..137, 160..178, 196..208].flatten_ranges.shuffle

@scary_light_colors = [124..125, 130..130,
                       160..161, 166..167, 172..172, 196..197,
                       202..203, 208..210, 220..221].flatten_ranges.shuffle

@scary_dark_colors = [124..125, 160..162,
                      166..168, 196..198, 202..204, 208..209].flatten_ranges.shuffle

def hash_chars(string)
	string.split('').inject{|sum, char| (sum.to_i ^ char[0]) % 131 }
end

def hostname_to_color(hostname)
	if @hostnames[hostname]
		if @options[:scary]
			colors = @options[:color]  == :light ? @scary_light_colors : @scary_dark_colors
			colors[hash_chars(hostname) % colors.length]
		else
			@hostnames[hostname]
		end
	else
		if @options[:scary]
			colors = @options[:color]  == :light ? @scary_light_colors : @scary_dark_colors
		else
			colors = @options[:color]  == :light ? @light_colors : @dark_colors
		end
		colors[hash_chars(hostname) % colors.length]
	end
end
if ARGV.first =~ /-/
	OptionParser.new do |opts|
		opts.banner = "Usage: #{__FILE__} [@options] string_to_hash"

		opts.on("-l", "--light", "Displays a light color (for dark color schemes) (default)") do
			@options[:color] = :light
		end

		opts.on("-d", "--dark", "Displays a dark color (for light color schemes)") do
			@options[:color] = :dark
		end

		opts.on("-s", "--scary-color", "Picks from a list of scary red/yellow/orange colors only") do
			@options[:scary] = true
		end

		opts.on("-h", "--hostnames", "Lists all saved hostnames, suitable for parsing by bash completion") do
			@options[:hostnames] = true
		end

		opts.on("-e", "--[no-]escape-code", "Prints xterm escape code instead of just numeric color code") do |escape|
			@options[:escape] = escape
		end
	end.parse!
end

if @options[:hostnames]
	print @hostnames.keys.join(' ')
else
	hostname = ARGV.first

	if hostname.nil?
		puts "Please specify a hostname"
		exit
	end

	colorcode = hostname_to_color(hostname).to_s

	if @options[:escape]
		print '\[\033[38;5;' + colorcode + 'm\]'
	else
		puts colorcode
	end
end
