label start  
	loco 5
	call fib    
	stod 4094
	stop
label fib
	subd  two
	jneg  neg
	push          
	addd  one
	call  fib    
	push               
	lodl  1            
	call  fib
	addl  0            
	insp  2            
	retn               
label neg
	loco  1            
	retn               
label one
	const 1            
label two
	const 2