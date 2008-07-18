require 'rubygems'
require 'wirble'
require 'utility_belt'
require 'what_methods'

def e
	edit(:kate)
end

IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:EVAL_HISTORY] = 1000

# Prompts
IRB.conf[:IRB_RC] = proc do |conf|
	dashes = "-" * conf.irb_name.length
	spaces = " " * conf.irb_name.length
	conf.prompt_n = dashes + "-("
	conf.prompt_i = "--( "
	conf.prompt_s = dashes + "-( %l"
	conf.prompt_c = dashes + "-( "
	conf.return_format   = dashes + "( %s )\n\n"
end


# Simple ri integration
def ri(*names)
  system("ri #{names.map {|name| name.to_s}.join(" ")}")
end


def time(times = 1)
	require 'benchmark'
	ret = nil
	Benchmark.bm { |x| x.report { times.times { ret = yield } } }
	ret
end

# UtilityBelt::Themes.background(:dark)

# from: http://raganwald.com/source/unfold.rb.html
class Object
	# unfold takes a "seed" argument and a incrementor. It returns an array. The
	# first element of the array is the seed, every subsequentelement is the
	# result of applying the incrementor to the previous element. so in
	# pretentious quasi-math: result[n] = incrementor(result[n-1]). the array
	# ends when the incrementor returns nil, so unfold(0) { |n| n+1 } is a bad
	# idea in Ruby to add terminating conditions, use if not logic, because false
	# does not terminate unfold.
	#
	# example: 10.unfold { |n| n-1 unless n == 1 }.inspect => [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
	# example: 10.class.unfold(&:superclass).inspect => [Fixnum, Integer, Numeric, Object] # using Symbol#to_proc

	# See also: NilClass#unfold
	def unfold_slow options = {}, &incrementor
		return [] unless options[:while].nil? || options[:while].to_proc.call(self)
		transformed = options[:map] && options[:map].to_proc[self] || self
		return [transformed] if options[:to] && options[:to].to_proc.call(self)
		incrementor.call(self).unfold(options, &incrementor).unshift(transformed)
	end

	# from: http://weblog.raganwald.com/2007/11/really-simple-anamorphisms-in-ruby.html
	# As above, but iterative, rather than recursive.
	def unfold &block
		result = [self]
		x = block.call(self)
		while not x.nil?
		result.push x
		x = block.call(x)
		end
		return result
	end
end

class NilClass
	# See: Object#unfold
	def unfold options = {}, &incrementor
		[]
	end
end

# local_methods shows methods that are only available for a given object.
class Object
    # Return a list of methods defined locally for a particular object.  Useful
    # for seeing what it does whilst losing all the guff that's implemented
    # by its parents (eg Object).
    def local_methods(obj = self)
        obj.methods(false).sort
    end
end

# Simple regular expression helper
# show_regexp - stolen from the pickaxe
def show_regexp(a, re)
	if a =~ re
		"#{$`}|#{$&}|#{$'}"
	else
		"no match"
	end
end

# Convenience method on Regexp so you can do
# /an/.show_match("banana")
class Regexp
	def show_match(a)
		show_regexp(a, self)
	end
end

puts "Ruby #{RUBY_VERSION} #{RUBY_PLATFORM}, PID: #{$$}, #{Time.now}"

