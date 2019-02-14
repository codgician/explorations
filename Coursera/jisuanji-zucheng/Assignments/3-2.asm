# Created by Shijia Zhang (@codgician)
# Feb 14, 2019
# Obviously I prefer mips over any valentine.

.data

successMsg: .asciiz "Success! Location: "
failedMsg: .asciiz "Fail!"
buffer: .space 1024

.text

readStr:
	li $v0, 8		# Set syscall to read string
	la $t0, buffer		# t0 = head address of buffer
	move $a0, $t0		# a0 = t0
	li $a1, 255		# Read at most 255 characters
	syscall			# Read String
	
readChar:
	li $v0, 12		# Set syscall to read character
	syscall			# Read query character
	move $t2, $v0		# t2 = v0
	
	li $t3, 63		# t3 = '?'
	beq $t2, $t3, exit	# Exit if t2 == '?'
	
	li $v0, 11		# Set syscall to print character
	li $a0, 10		# a0 = '\n'
	syscall
	
	move $t1, $t0		# t1 = t0

search:
	lb $t3, ($t1)		# t3 = current character
	beqz $t3, failed	# If reaches the end, then search failed
	beq $t3, $t2, success	# If t3 == t2, then search succeeded
	addi $t1, $t1, 1	# t1 = t1 + 1
	j search
	
failed:
	li $v0, 4		# Set syscall to print string
	la $a0, failedMsg	# t0 = head address of failedMsg
	syscall			# Print failure message
	j loop

success:
	li $v0, 4		# Set syscall to print string
	la $a0, successMsg	# Print success message
	syscall

	li $v0, 1		# Set syscall to print integer
	sub $t1, $t1, $t0	# t1 = t1 - t0
	addi $t1, $t1, 1	# t1 = t1 + 1
	move $a0, $t1		# a0 = t1
	syscall
	
loop:
	li $v0, 11		# Set syscall to print character
	li $a0, 10		# a0 = '\n'
	syscall			# Print newline
	j readChar
	
exit:	# Terminate execution
	li $v0, 10
	syscall
