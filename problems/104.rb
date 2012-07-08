# Finds the first k so that the Fibonacci number F_k's first and last nine
# digits are both 1-9 pandigital
# solution == 329468
$cache = [0,1]
def fib n
	$cache[n] ||= fib(n-1) + fib(n-2)
end

class Integer
	def pandigital?
		self.to_s.chars.sort.join == '123456789'
	end

	def works?
		string = self.to_s
		string[0, 9].to_i.pandigital? and string[-9, string.length].to_i.pandigital?
	end
end

def solution
	i = 2749
	until fib(i).works?
		i += 1
	end
	i
end