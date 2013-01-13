class Integer
	def reduce
		# Regex slice to get rid of the trailing (well, leading) zeroes
		# Reversing the string makes the regex considerably simpler as it can just
		# be greedy
		self.to_s.reverse[/0*(\d*)/, 1][0..4].reverse.to_i
	end
end

def f k
	# We essentially want to do (2..k).inject { |m, n| (m*n).reduce }, but
	# creating (2..1_000_000_000_000) uses up too much memory, so we'll loop:
	term = 2  # The current term to multiply
	prod = 1  # The running reduced product
	while term <= k
		prod = (prod * term).reduce
		term += 1
	end
	prod
end