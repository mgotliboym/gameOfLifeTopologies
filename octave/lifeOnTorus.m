#
#width=128;
#heightMultiplier=1;
#gameHeight = 50;
#height = gameHeight*heightMultiplier;
#

function lifeOnTorus(width, gameHeight, heightMultiplier, uspf)
  i=0;
  height = gameHeight*heightMultiplier;
  
  ##for i = 0:65
  while 1 
    ##Read file
    M = dlmread("outputGoL", ' ', [i*(gameHeight+1), 0, i*(gameHeight+1) + gameHeight-1, width-1]); #(i+1)*(gameHeight)-1, (width-1)]); #read one generation
    ##Add col to M
    M = [M ones(rows(M), 1)];
    ##magic matrix math to duplicate rows of the matrix to fill the board vertically
    CellVals = kron(M, ones(heightMultiplier,1));
    ##Add row to M
    CellVals = [CellVals; ones(1,columns(CellVals))];

    ##Create torus
    u=0:2*pi/(width):2*pi;
    v=0:2*pi/(height):2*pi;
    [u,v] = meshgrid(u,v);

    a=3;
    b=1;

    X=(a+b.*cos(v)).*cos(u);
    Y=(a+b.*cos(v)).*sin(u);
    Z=b.*sin(v);

    ##CellVals = randi([0 0], height, width); # random cell values for testing

    s=surf(X,Y,Z, CellVals, 'cDataMapping', 'direct');
    ##s.cDataMapping='direct';
    colormap([1  1  0; 0  1  1]); #Specifies the two colors to use, 0 is second.
    ##rotate(s, [1,1,0], 30);
    view(322,70);
    
    usleep(uspf);
    i=i+1;
  end
endfunction

lifeOnTorus(129, 5, 8, 300000);

ans = input("Hit enter to close");
