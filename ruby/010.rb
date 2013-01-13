require 'prime'

def solution
  Prime.take_while { |p| p< 2_000_000 }.sum
end