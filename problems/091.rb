# Finds the number of right triangles with vertices at (0,0), (a,b), (c,d) for
# 0 <= a,b,c,d <= 50
# solution == 14234
def is_right? x1, y1, x2, y2
  sides = [
  	x1**2 + y1**2,
  	x2**2 + y2**2,
  	(x2-x1)**2 + (y2-y1)**2
  ].sort
  sides[0] != 0 and sides[0] + sides[1] == sides[2]
end

def solution dim=50
	count = 0
	for x1 in (0..dim)
		for y1 in (0..dim)
			for x2 in (0..dim)
				for y2 in (0..dim)
					if is_right? x1, y1, x2, y2
						count += 1
					end
				end
			end
		end
	end
	count / 2  # This double-counts, by symmetry
end