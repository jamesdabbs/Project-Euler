# Finds the sum of all palindromes (below 10**8) which can be written as a sum
# of consecutive squares
# solution == 2906969179
require 'set'

def sum_of_squares(a, b)
	# 1^2 + 2^2 + ... + n^2 = n * (n+1) * (2n+1) / 6. It is known.
	# Thus a^2 + ... b^2 = (1^2 + ... + b^2) - (1^2 + (a-1)^2) =
  (b * (b+1) * (2*b+1))/6 - ((a-1) * a * (2*a-1))/6
  # (At least, assuming a < b)
end

class Integer
	def is_palindrome?
		return self.to_s == self.to_s.reverse
	end
end

def solution(places=8)
	a = 1
	hits = Set.new
	while a < 10**((places+1)/2)  # Integer division, rounded up
		b = a+1
		sum = sum_of_squares a, b
		while sum < 10**places
			if sum.is_palindrome?
				hits.add sum
			end
			b += 1
			sum = sum_of_squares a, b
		end
		a += 1
	end
	hits.to_a.inject { |acc,x| acc + x}
end