// -*- Mode: scilab -*- 
// Copyright 2013-2015 Enpc
//
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Authors: J.-Ph. Chancelier (Cermics/ENPC)

 

function y=f(z);
  y=Select_exp(z,ones(size(z)),eye(size(z)),zeros(size(z)));
endfunction;

function y=code_test_data()
  y=list(grand(4,3,'uin',1,3));
endfunction;

