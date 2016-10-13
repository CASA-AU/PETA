#
#     Copyright (C) 2015  Civil Aviation Safety Authority
# 
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License along
#     with this program; if not, write to the Free Software Foundation, Inc.,
#     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#

fnPatchVarname <- function(x) {
	
	## replace colons with periods and spaces with underscore
	x <- gsub(' ', '_', gsub(' : ', '\\.', x))
	## replace '/' with word 'or'
	x <- gsub('_*/_*', '_or_', x)
	## replace hyphens with underscores - hyphens get interpretted as minus sign in formula
	x <- gsub('-', '_', x)
	## replace commas with underscores
	x <- gsub(',', '.', x)
	## remove brackets
	x <- gsub('\\(|\\)', '', x)	
	## replace ampersand (&) with word 'and'
	x <- gsub('\\&', 'and', x)	
	
	return(x)
}