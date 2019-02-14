# Created by Shijia Zhang (@codgician)
# Feb 14, 2019
# Obviously I prefer mips over any valentine.

.data

letterText: .asciiz "lpha", "ravo", "hina", "elta", "cho", "oxtrot", "olf", "otel", "ndia", "uliet", "ilo", "ima", "ary", "ovember", "scar", "aper", "uebec", "esearch", "ierra", "ango", "niform", "ictor", "hisky", "-ray", "ankee", "ulu"
letterOffset: .word 0, 5, 10, 15, 20, 24, 31, 35, 40, 45, 51, 55, 59, 63, 71, 76, 81, 87, 95, 101, 106, 113, 119, 125, 130, 136
digitText: .asciiz "zero", "First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth"
digitOffset: .word 0, 5, 11, 18, 24, 31, 37, 43, 51, 58

.text

main:

read:	# Read character using syscall and classify it
	li $v0, 12		# Set syscall to read character
	syscall			# Read input character to v0
	
	move $t0, $v0		# t0 = v0
	
	li $t1, 63		# t1 = '?'
	beq $t0, $t1, exit	# Exit if t0 == '?'
	
	li $v0, 11		# Set syscall to print character
	li $a0, 10		# a0 = '\n'
	syscall			# Print newline
	
	li $t1, 48		# t1 = '0'
	blt $t0, $t1, unknown	# If t0 < '0', then goto unkown
	li $t1, 58		# t1 = ':' (next character of '9')
	blt $t0, $t1, digit	# If t0 >= '0' && t0 <= '9', then goto digit
	li $t1, 65		# t1 = 'A'
	blt $t0, $t1, unknown	# If t0 > '9' && t0 < 'A', then goto unkown
	li $t1, 91		# t1 = '[' (next character of 'Z')
	blt $t0, $t1, upper 	# If t0 >= 'A' && t0 <= 'Z', then goto letter
	li $t1, 97		# t1 = 'a'
	blt $t0, $t1, unknown	# If t0 > 'Z' && t0 < 'a', then goto unkown
	li $t1, 123		# t1 = '{' (next character of 'z')
	blt $t0, $t1, lower	# If t0 >= 'a' && t0 <= 'z', then goto letter
	
unknown:# Handle unkown input 
	li $v0, 11		# Set syscall to print character
	li $a0, 42		# a0 = '*'
	syscall
	j loop
	
upper:	# Calculate index (upper-case letter)
	subi $t1, $t0, 65	# t1 = t0 - 'A'	
	j letter
	
lower:	# Calculate index for (lower-case letter)
	subi $t1, $t0, 97
	j letter
	
letter:	# Calculate address of result text (letter)
	li $v0, 11		# Print itself first
	move $a0, $t0
	syscall
	
	sll $t1, $t1, 2		# t1 = t1 << 2 (A word has 4 bytes)
	la $t0, letterOffset	# t0 = start address of letterOffset
	add $t1, $t1, $t0	# t1 = t1 + t0
	lw $t0, ($t1)		# t0 = value in address (t1)
	
	la $t1, letterText	# t1 = start address of letterText
	add $t1, $t1, $t0	# t1 = t1 + t0

	j print			

digit:	# Calculate address if result text (digit)
	subi $t1, $t0, 48	# t0 = t0 - '0'
	sll $t1, $t1, 2		# t0 = t0 << 2 (A word has 4 bytes)
	la $t0, digitOffset	# t0 = start address of digitOffset
	add $t1, $t1, $t0	# t1 = t1 + t0
	lw $t0, ($t1)		# t0 = value in address (t1)
	
	la $t1, digitText	# t1 = start address of digitText
	add $t1, $t1, $t0	# t1 = t1 + t0

	j print
	
print:	# Print result text
	li $v0, 4		# Set syscall to print string
	move $a0, $t1		# a0 = t1
	syscall			# Print the rest of the text
	j loop	

loop:
	li $v0, 11		# Set syscall to print character
	li $a0, 10		# a0 = '\n'
	syscall
	j main
	
exit:	# Terminate execution
	li $v0, 10
	syscall
