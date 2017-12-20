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

 

function [xhat,P,K]=f(xhat,P,K,meas,dt)
  Q=diag([0 1. 0 1.]);
  R=diag([50^2 0.005^2]);
  // Calculate the Jacobians for the state and measurement equations
  F = [1 dt 0 0;0 1 0 0;0 0 1 dt;0 0 0 1];
  rangeHat = sqrt(xhat(1)^2+xhat(3)^2);
  bearingHat = atan(xhat(3),xhat(1));
  yhat = [rangeHat; bearingHat];
  H = [cos(bearingHat) 0 sin(bearingHat) 0;
       -sin(bearingHat)/rangeHat 0 cos(bearingHat)/rangeHat 0];
  // Propogate the state and covariance matrices
  xhat = F*xhat;
  P = F*P*F' + Q;
  // Calculate the Kalman gain
  K = P*H' * inv((H*P*H' + R));
  // K = ((H*P*H' + R)'\ (P*H')')';
  // Calculate the measurement residual
  resid = meas - yhat;
  // Update the state and covariance estimates
  xhat = xhat + K*resid;
  P = (eye(size(K,1))-K*H)*P;
  // Post the results
  xhatOut = xhat;
endfunction


function y=code_test_data()
  y=list(rand(4,1),rand(4,4),rand(4,2),rand(2,1),0.1);
endfunction;

