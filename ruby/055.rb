# Finds the number of Lychrel numbers below 10000
# solution == 249
Max_depth = 50

class Integer
	def is_palindrome?
		string = self.to_s
		string == string.reverse
	end

	def lychrel_number depth=0
		# This memoizes the lychrel number
		@lyrchrel_number ||= begin
			step = self + self.to_s.reverse.to_i
			if step.is_palindrome?
				1
			elsif depth > Max_depth
				nil
			else
				step_num = step.to_i.lychrel_number(depth + 1)
				step_num ? step_num + 1 : nil
			end
		end
	end
end

def solution
	(1..10_000).find_all { |n| n.lychrel_number == nil }.length
end