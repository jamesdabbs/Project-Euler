# Finds the number of numbers below 10,000,000 such that iterating
#   "abcd..." -> a^2 + b^2 + c^2 + d^2 + ...
# ends up reaching 89 (and not 1)
# solution == 8581146
def sum_of_digits x
	x.to_s.chars.inject(0) {|sum,d| sum + (d.to_i)**2}
end

$cache = {1 => 1, 89 => 89}
def sink x
	$cache[x] ||= sink(sum_of_digits x)
end

def solution max=10_000_000
	a = 1
	count = 0
	while a < max
		if sink(a) == 89
			count += 1
		end
		a += 1
	end
	count
end