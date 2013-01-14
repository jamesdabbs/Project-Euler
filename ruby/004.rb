def palindrome?(n)
  n.to_s.reverse == n.to_s
end

def solution
  palindromes = Set.new

  999.downto(100) do |a|
    999.downto(a) do |b|
      prod = a*b
      palindromes << prod if palindrome?(prod)
    end
  end

  palindromes.max
end