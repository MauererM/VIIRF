% MIT License
%
% Copyright (c) 2017 Mario Mauerer
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.
%
% VIIRF - Versatile IIR Filter
%
% This function writes matlab-generated SOS/G matrices into a file as python lists,
% Such that it can be utilized for the configuration-script IIR_Config.py.
%
% filename:
%   String of desired output-file. Don't forget ".txt"
% sos:
%   Matrix containing the SOS-coefficients. Size: [numsec 6]
% g:
%   Vector containing the section-gains and/or the final filter gain.
%   If the filter does not use any gains, this can be an empty vector.
% Example-usage:
%   export_sos_python_lists('LPFilt.txt', SOSDat, GDat);

function [] = export_sos_python_lists(filename, sos, g)

l = size(sos);
numsec = l(1);

fid = fopen(filename,'w'); % Create the file or overwrite an existing file

% First, the SOS-coefficients:
fprintf(fid, 'SOS = [');
for sec = 1:numsec
    fprintf(fid, '[');
    for c = 1:6
        fprintf(fid, '%.18f', sos(sec, c));
        if c < 6
            fprintf(fid, ', ');
        end
    end
    fprintf(fid, ']');
    if sec < numsec
        fprintf(fid, ',\n\t\t');
    end
end
fprintf(fid, ']');

% The G-Vector:
fprintf(fid, '\n\n');
fprintf(fid, 'G = [');
for sec = 1:length(g)
    fprintf(fid, '%.18f', g(sec));
    if sec < length(g)
        fprintf(fid, ', ');
    end
end
fprintf(fid, ']');
fclose(fid); % We're done

end