
module mips(input  logic        clk, reset,
            output logic [31:0] pcF,
            input  logic [31:0] instrF,
            output logic        memwriteM,
            output logic [31:0] aluoutM,fpuoutM ,final_writedata,
            input  logic [31:0] readdataM);
 logic[31:0] writedataM;			
 logic [5:0]  opD, functD;
 logic        regdstE, alusrcE,
             pcsrcD,
             memtoregE, memtoregM, memtoregW, 
             regwriteE, regwriteW
             ,float_memtoregE,
			 float_memtoregM,
			 float_memtoregW,
			 float_regwriteE,
			 float_regwriteM,
			 float_regwriteW,
			 float_regdstE,
			 fpusrcE,
       float_memwriteM;
 logic [2:0]  alucontrolE;
 logic [3:0]  fpucontrolE;
 logic        flushE, equalD, nequalD;
 
 controller c(clk, reset, opD, functD, flushE, 
              equalD, nequalD, memtoregE, memtoregM, 
              memtoregW, memwriteM, pcsrcD, 
              branchD, bneD, alusrcE, regdstE, regwriteE,
              regwriteM, regwriteW, jumpD, extendD,
              alucontrolE
             ,float_memtoregE,
			 float_memtoregM,
			 float_memtoregW,
			 fpusrcE,
			 float_regdstE,
			 float_regwriteE,
			 float_regwriteM,
			 float_regwriteW,
			 fpucontrolE,
       float_memwriteM
              );
			  
 datapath dp(clk, reset, memtoregE, memtoregM, 
             memtoregW, pcsrcD, branchD, bneD,
             alusrcE, regdstE, regwriteE, 
             regwriteM, regwriteW, jumpD, extendD,
             alucontrolE,
             equalD, nequalD, pcF, instrF,
             aluoutM, writedataM,readdataM,
             opD, functD, flushE,
               float_memtoregE,
			   float_memtoregM,
			   float_memtoregW,
			   fpusrcE,
			   float_regdstE,
			   float_regwriteE,
			   float_regwriteM,
			   float_regwriteW,
			   fpucontrolE,
		//
		       fpuoutM	   ,
			   final_writedata
         ,float_memwriteM
 
             );
endmodule



module controller(input   logic        clk, reset,
                  input   logic  [5:0] opD, functD,
                  input   logic        flushE, equalD, nequalD,
                  output  logic        memtoregE, memtoregM, 
                  output  logic       memtoregW, memwriteM,
                  output  logic        pcsrcD, branchD, bneD, alusrcE,
                  output  logic       regdstE, regwriteE, 
                  output  logic       regwriteM, regwriteW,
                  output  logic       jumpD, extendD,
                  output  logic [2:0] alucontrolE
                  ,
                  output  logic float_memtoregE,
		        	 	  output  logic float_memtoregM,
		        	 	  output  logic float_memtoregW,
          
                  output  logic fpusrcE,
			        	  output  logic float_regdstE,
	         		 	  output  logic float_regwriteE,
	         		 	  output  logic float_regwriteM,
	         		 	  output  logic float_regwriteW,
	        		 	  output  logic [3:0] fpucontrolE,
                  output  logic float_memwriteM
          );
				  
 logic [1:0] aluopD;
 logic [2:0] fpuopD;
 
 logic       memtoregD, memwriteD, alusrcD,
            regdstD, regwriteD,
  		float_memtoregD,
			fpusrcD,
			float_regdstD,
			float_regwriteD,
      float_memwriteD, 
      float_memwriteE 
  ;

 logic [2:0] alucontrolD;
 logic [3:0] fpucontrolD;
 logic       memwriteE;
 maindec md(opD, memtoregD, memwriteD, branchD, bneD,
            alusrcD, regdstD, regwriteD, jumpD, extendD,
            aluopD,

			float_memtoregD,
			fpusrcD,
			float_regdstD,
			float_regwriteD,
      float_memwriteD
			
            );
 aludec  ad(functD, aluopD, alucontrolD);
 fpudec  fpudecoder(functD, fpucontrolD);
 
 assign pcsrcD = (branchD & equalD) | (bneD & nequalD);
 
 // pipeline registers
 floprc #(17) regE(clk, reset, flushE,
                 {memtoregD, memwriteD, alusrcD, 
                  regdstD, regwriteD, alucontrolD,
              


                  float_memtoregD, fpusrcD, 
                  float_regdstD, float_regwriteD, fpucontrolD,float_memwriteD}, 
                 


                 {memtoregE, memwriteE, alusrcE, 
                  regdstE, regwriteE,  alucontrolE
                  

                  ,
                  float_memtoregE, fpusrcE, 
                  float_regdstE, float_regwriteE,  fpucontrolE,float_memwriteE});


 flopr #(6) regM(clk, reset, 
                 {memtoregE, memwriteE, regwriteE

					,
				  float_memtoregE, float_regwriteE,float_memwriteE},

                 {memtoregM, memwriteM, regwriteM,
                 float_memtoregM, float_regwriteM,float_memwriteM});


 flopr #(4) regW(clk, reset, 
                 {
                 memtoregM, regwriteM
                 ,
                 float_memtoregM, float_regwriteM
                 },


                 {
                 memtoregW, regwriteW,
                 float_memtoregW, float_regwriteW
                 }
                 );
endmodule



 

module maindec(input   logic  [5:0] op,
               output  logic       memtoreg, memwrite,
               output  logic       branch, bne, alusrc,
               output  logic       regdst, regwrite,
               output  logic       jump, extend,
               output  logic [1:0] aluop,
               output  logic float_memtoreg,
			   output  logic fpusrc,
			   output  logic float_regdst,
			   output  logic float_regwrite,
         float_memwrite

			   );

 logic [15:0] controls;
 assign {regwrite, regdst, alusrc,
         branch, bne, memwrite,
         memtoreg, jump, extend, aluop,float_memtoreg,fpusrc,float_regdst,float_regwrite,float_memwrite} = controls;

 always_comb
   case(op)
     6'b000000: controls <= 16'b11000000011__00000; //Rtyp
     6'b010001: controls <= 16'b00000000000__00110; //fRtyp//fpusrc ??
     6'b110001: controls <= 16'b00100010100__11010; //flw  /////??
     6'b111001: controls <= 16'b00100100100__01001; //fst   ///??
     6'b100011: controls <= 16'b10100010100__00000; //LW
     6'b101011: controls <= 16'b00100100100__00000; //SW
     6'b000100: controls <= 16'b00010000001__00000; //BEQ
     6'b001000: controls <= 16'b10100000100__00000; //ADDI
     6'b000010: controls <= 16'b00000001000__00000; //J
  	 6'b000101: controls <= 16'b00001000001__00000; //BNE
     6'b001101: controls <= 16'b10100000010__00000; //ORI
	   default:   controls <= 16'bxxxxxxxxxxxxxxxx; //???
   endcase
endmodule



module aludec(input   logic   [5:0] funct,
              input   logic   [1:0] aluop,
              output  logic  [2:0] alucontrol);

 always_comb
   case(aluop)
     2'b00: alucontrol <= 3'b010;  // add (for lw/sw/addi) 
     2'b01: alucontrol <= 3'b110;  //  sub (for beq/bne)
     2'b10: alucontrol <= 3'b001;  //or (for ori)
	 default: case(funct)          // RTYPE
         6'b100000: alucontrol <= 3'b010; // ADD
         6'b100010: alucontrol <= 3'b110; // SUB
         6'b100100: alucontrol <= 3'b000; // AND
         6'b100101: alucontrol <= 3'b001; // OR
         6'b101010: alucontrol <= 3'b111; // SLT
         default:   alucontrol <= 3'bxxx; // ???
       endcase
   endcase
endmodule




module fpudec(input   logic   [5:0] funct,
       //       input   logic   [2:0] fpuop,
              output  logic   [3:0] fpucontrol);

assign fpucontrol=funct;	
 /*always_comb
   case(funct)
   	////complete
     //funct >>>> control 
   endcase
endmodule
*/
endmodule
module datapath(input   logic         clk, reset,
                input   logic         memtoregE, memtoregM, memtoregW, 
                input   logic         pcsrcD, branchD, bneD,
                input   logic         alusrcE, regdstE,
                input   logic         regwriteE, regwriteM, regwriteW, 
                input   logic         jumpD, extendD,
                input   logic  [2:0]  alucontrolE,
                output  logic        equalD, nequalD,
                output  logic [31:0] pcF,
                input   logic  [31:0] instrF,
                output  logic [31:0] aluoutM, writedataM,
                input   logic  [31:0] readdataM,
                output  logic [5:0]  opD, functD,
                output  logic        flushE,
                input logic float_memtoregE,
			    input logic float_memtoregM,
			    input logic float_memtoregW,
			    input logic fpusrcE,
			    input logic float_regdstE,
			    input logic float_regwriteE,
			    input logic float_regwriteM,
			    input logic float_regwriteW,
			    input logic[3:0] fpucontrolE,
			    output logic[31:0] fpuoutM,final_writedata,
          input logic float_memwriteM

			    //fpuoutm 
		//	   output logic final_writedata

                );

 logic        forwardaD, forwardbD;

 logic        float_forwardaD, float_forwardbD;

 logic [1:0]  forwardaE, forwardbE;
 
logic [1:0]  float_forwardaE, float_forwardbE;
 

 logic        stallF;
 logic [4:0]  rsD, rtD, rdD, rsE, rtE, rdE;
 logic [4:0]  float_ftD, float_fsD, float_fdD, float_ftE, float_fsE, float_fdE;
 logic [4:0]  writeregE, writeregM, writeregW;
 
 logic [4:0]  float_writeregE, float_writeregM, float_writeregW;
 
 logic        flushD;
 logic [31:0] pcnextFD, pcnextbrFD, pcplus4F, pcbranchD;
 logic [31:0] signimmD, zeroimmD, immD, immE, signimmshD;
 logic [31:0] srcaD, srca2D, srcaE, srca2E;

 logic [31:0] float_srcaD,  float_srcaE,  float_srca2E ;
 
 logic [31:0] srcbD, srcb2D, srcbE, srcb2E, srcb3E;
 
 logic [31:0] float_srcbD, float_srcb2D, float_srcbE, float_srcb2E;
 
 logic [31:0] pcplus4D, instrD;
 logic [31:0] aluoutE, aluoutW;
 logic [31:0] fpuoutE, fpuoutW,float_resultW;
 logic [31:0] readdataW, resultW;
 logic [31:0] float_writedataM;
//logic[31:0]float_readdataM, float_readdataW;//
 // hazard detection
 hazard    h(rsD, rtD, rsE, rtE, writeregE, writeregM, 
             writeregW,regwriteE, regwriteM, regwriteW, 
             memtoregE, memtoregM, branchD, bneD,
             forwardaD, forwardbD, forwardaE, 
             forwardbE,
             stallF, stallD, flushE
			,
			
			float_ftD, float_fsD, float_ftE, float_fsE
			,
			 float_writeregE, float_writeregM, 
       float_writeregW,float_regwriteE, float_regwriteM, float_regwriteW, 
       float_memtoregE, float_memtoregM,
 			 float_forwardaD, float_forwardbD, float_forwardaE, 
       float_forwardbE
             );

 // next PC logic (operates in fetch and decode)
 mux2 #(32)  pcbrmux(pcplus4F, pcbranchD, pcsrcD, 
                     pcnextbrFD);
 mux2 #(32)  pcmux(pcnextbrFD,{pcplus4D[31:28], 
                   instrD[25:0], 2'b00}, 
                   jumpD, pcnextFD);

 // register file (operates in decode and writeback)
 regfile     rf(clk, regwriteW, rsD, rtD, writeregW,
                resultW, srcaD, srcbD);
 regfile     float_rf(clk, float_regwriteW, float_fsD, float_ftD, float_writeregW,
                float_resultW, float_srcaD, float_srcbD);
 
// Fetch stage logic
 flopenr #(32) pcreg(clk, reset, ~stallF, 
                     pcnextFD, pcF);
 adder       pcadd1(pcF, 32'b100, pcplus4F);

// Decode stage 
 flopenr  #(32) r1D(clk, reset, ~stallD, pcplus4F,pcplus4D);
 flopenrc #(32) r2D(clk, reset, ~stallD, flushD, instrF, instrD);
 signext        se(instrD[15:0], signimmD);
 zeroext        ze(instrD[15:0], zeroimmD);
 mux2 #(32)     ext(zeroimmD, signimmD, extendD, immD);
 
 sl2         immsh(signimmD, signimmshD);
 adder       pcadd2(pcplus4D, signimmshD, pcbranchD);
 mux2 #(32)  forwardadmux(srcaD, aluoutM, forwardaD,srca2D);
 mux2 #(32)  forwardbdmux(srcbD, aluoutM, forwardbD,srcb2D);
 eqcmp       comp(srca2D, srcb2D, equalD);
 neqcmp      ncom(srca2D, srcb2D, nequalD);
 
 assign opD = instrD[31:26];
 assign functD = instrD[5:0];
 assign rsD = instrD[25:21];
 assign rtD = instrD[20:16];
 assign rdD = instrD[15:11];
 
 assign float_ftD = instrD[20:16];
 assign float_fsD = instrD[15:11];
 assign float_fdD = instrD[10:6];
 
 assign flushD = (pcsrcD & ~stallD) | jumpD;

 
 // Execute stage 
 floprc #(32) r1E(clk, reset, flushE, srcaD, srcaE);
 floprc #(32) r2E(clk, reset, flushE, srcbD, srcbE);
 floprc #(32) r3E(clk, reset, flushE, immD, immE);
 floprc #(5)  r4E(clk, reset, flushE, rsD, rsE);
 floprc #(5)  r5E(clk, reset, flushE, rtD, rtE);
 floprc #(5)  r6E(clk, reset, flushE, rdD, rdE);

 floprc #(32) float_r1E(clk, reset, flushE, float_srcaD, float_srcaE);
 floprc #(32) float_r2E(clk, reset, flushE, float_srcbD, float_srcbE);
 floprc #(5)  float_r4E(clk, reset, flushE, float_ftD, float_ftE);
 floprc #(5)  float_r5E(clk, reset, flushE, float_fsD, float_fsE);
 floprc #(5)  float_r6E(clk, reset, flushE, float_fdD, float_fdE);

 mux3 #(32)  forwardaemux(srcaE, resultW, aluoutM, forwardaE, srca2E);
 mux3 #(32)  forwardbemux(srcbE, resultW, aluoutM, forwardbE, srcb2E);
 mux2 #(32)  srcbmux(srcb2E, immE, alusrcE, srcb3E);




 mux3 #(32)  float_forwardaemux(float_srcaE, float_resultW, fpuoutM, float_forwardaE, float_srca2E);
 mux3 #(32)  float_forwardbemux(float_srcbE, float_resultW, fpuoutM, float_forwardbE, float_srcb2E);


 alu         alu(srca2E, srcb3E, alucontrolE, aluoutE);
 mux2 #(5)   wrmux(rtE, rdE, regdstE, writeregE);
 
 fpu   fpu( clk,
             fpucontrolE ,       
             float_srca2E,float_srcb2E, 
            fpuoutE);

mux2 #(5)   float_wrmux(float_ftE, float_fdE, float_regdstE, float_writeregE);

 // Memory stage
 flopr #(32) r1M(clk, reset, srcb2E, writedataM);
 flopr #(32) r2M(clk, reset, aluoutE, aluoutM);
 flopr #(5)  r3M(clk, reset, writeregE, writeregM);
 
 //flopr #(32) floaT_r1M(clk, reset, float_srcb2E, final_writedata);
 

 flopr #(32) floaT_r2M(clk, reset, fpuoutE, fpuoutM);
 flopr #(5)  float_r3M(clk, reset, float_writeregE, float_writeregM);
 flopr #(32) float_r4M(clk, reset, float_srcb2E, float_writedataM);
 
 mux2 #(32) finalwrite(writedataM,float_writedataM,float_memwriteM,final_writedata);

 // Writeback stage
 flopr #(32) r1W(clk, reset, aluoutM, aluoutW);
 flopr #(32) r2W(clk, reset, readdataM, readdataW);
 flopr #(5)  r3W(clk, reset, writeregM, writeregW);
 mux2 #(32)  resmux(aluoutW, readdataW, memtoregW, resultW);

flopr #(32) float_r1W(clk, reset, fpuoutM, fpuoutW);
//flopr #(32) float_r2W(clk, reset, float_readdataM, float_readdataW);
flopr #(5)  float_r3W(clk, reset, float_writeregM, float_writeregW);
mux2 #(32)  float_resmux(fpuoutW, readdataW, float_memtoregW, float_resultW);
 always_ff @(negedge clk)
 if (float_memwriteM)
 $display("true"); 
endmodule



// building blocks

module regfile(input  logic        clk, 
               input  logic        we3, 
               input  logic [4:0]  ra1, ra2, wa3, 
               input  logic [31:0] wd3, 
               output logic [31:0] rd1, rd2);

  logic [31:0] rf[31:0];

  // three ported register file
  // read two ports combinationally
  // write third port on rising edge of clk
  // register 0 hardwired to 0
  // note: for pipelined processor, write third port
  // on falling edge of clk

  always_ff @(negedge clk)
begin
    if (we3) rf[wa3] <= wd3;	
    $display("indx%d REGval:%d\n",wa3,wd3);
end
  assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule





module hazard(input   logic  [4:0] rsD, rtD, rsE, rtE, 
              input   logic  [4:0] writeregE, writeregM, writeregW,
              input   logic        regwriteE, regwriteM, regwriteW,
              input   logic        memtoregE, memtoregM, branchD, bneD,
              output  logic           forwardaD, forwardbD,
              output  logic [1:0] forwardaE, forwardbE,
              output  logic       stallF, stallD, flushE,
              input   logic  [4:0] float_ftD, float_fsD, float_ftE, float_fsE,
			  input   logic  [4:0] float_writeregE, float_writeregM,float_writeregW,
              input   logic        float_regwriteE, float_regwriteM, float_regwriteW, 
              input   logic        float_memtoregE, float_memtoregM,
 			  output  logic        float_forwardaD, float_forwardbD, 
              output  logic [1:0]  float_forwardaE,float_forwardbE);
			  
 logic lwstallD, branchstallD;
 logic float_lwstallD;
 // forwarding sources to D stage (branch equality)
 assign forwardaD = (rsD !=0 & rsD == writeregM & regwriteM);
 assign forwardbD = (rtD !=0 & rtD == writeregM & regwriteM);
 
 // forwarding sources to E stage (ALU)
 always_comb
   begin
     forwardaE = 2'b00; forwardbE = 2'b00;
     if (rsE != 0)
       if (rsE == writeregM & regwriteM) 
         forwardaE = 2'b10;
       else if (rsE == writeregW & regwriteW) 
         forwardaE = 2'b01;
     if (rtE != 0)
       if (rtE == writeregM & regwriteM) 
         forwardbE = 2'b10;
       else if (rtE == writeregW & regwriteW) 
         forwardbE = 2'b01;
   end
   
 // stalls  
 assign #1 lwstallD = memtoregE & 
                     (rtE == rsD | rtE == rtD);
 assign #1 branchstallD = (branchD | bneD) & 
            (regwriteE & 
            (writeregE == rsD | writeregE == rtD) |
             memtoregM & 
            (writeregM == rsD | writeregM == rtD));
 assign #1 stallD = lwstallD | branchstallD|float_lwstallD;
 assign #1 stallF = stallD;
   // stalling D stalls all previous stages
 assign #1 flushE = stallD; 
   // stalling D flushes next stage
 // Note: not necessary to stall D stage on store 
 //       if source comes from load;
 //       instead, another bypass network could 
 //       be added from W to M




//assign float_forwardaD = (float_fsD !=0 & float_fsD == float_writeregM & float_regwriteM);
//assign float_forwardbD = (float_ftD !=0 & float_ftD == float_writeregM & float_regwriteM);
 // forwarding sources to E stage (ALU)
 always_comb
   begin
     float_forwardaE = 2'b00; float_forwardbE = 2'b00;
     if (float_fsE != 0)
       if (float_fsE == float_writeregM & float_regwriteM) 
         float_forwardaE = 2'b10;
       else if (float_fsE == float_writeregW & float_regwriteW) 
         float_forwardaE = 2'b01;
     if (rtE != 0)
       if (float_ftE == float_writeregM & float_regwriteM) 
         float_forwardbE = 2'b10;
       else if (float_ftE == float_writeregW & float_regwriteW) 
         float_forwardbE = 2'b01;
   end
   
 // stalls  
 assign #1 float_lwstallD = float_memtoregE & 
                     (float_ftE == float_fsD | float_ftE == float_ftD);
 //assign #1 stallD = lwstallD | branchstallD;
 //assign #1 stallF = stallD;
   // stalling D stalls all previous stages
// assign #1 flushE = stallD; 
   // stalling D flushes next stage
 // Note: not necessary to stall D stage on store 
 //       if source comes from load;
 //       instead, another bypass network could 
 //       be added from W to M


endmodule



module adder(input  logic [31:0] a, b,
             output logic [31:0] y);

  assign y = a + b;
endmodule



module sl2(input  logic [31:0] a,
           output logic [31:0] y);

  // shift left by 2
  assign y = {a[29:0], 2'b00};
endmodule



module signext(input  logic [15:0] a,
               output logic [31:0] y);
              
  assign y = {{16{a[15]}}, a};
endmodule



module zeroext(input  logic [15:0] a,
               output logic [31:0] y);
              
  assign y = {16'b0, a};
endmodule



module flopr #(parameter WIDTH = 8)
              (input  logic             clk, reset,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if (reset) q <= 0;
    else       q <= d;
endmodule



module flopenr #(parameter WIDTH = 8)
              (input  logic             clk, reset, en,
               input  logic [WIDTH-1:0] d, 
               output logic [WIDTH-1:0] q);

  always_ff @(posedge clk, posedge reset)
    if (reset)   q <= 0;
    else if (en) q <= d;
endmodule



module floprc #(parameter WIDTH = 8)
              (input   logic  clk, reset, clear,
               input   logic    [WIDTH-1:0] d, 
               output  logic   [WIDTH-1:0] q);
 always_ff @(posedge clk, posedge reset)
   if      (reset) q <= 0;
   else if (clear) q <= 0;
   else            q <= d;
endmodule



module flopenrc #(parameter WIDTH = 8)
                (input   logic     clk, reset,
                 input   logic      en, clear,
                 input   logic  [WIDTH-1:0] d, 
                 output  logic [WIDTH-1:0] q);
 always_ff @(posedge clk, posedge reset)
   if      (reset) q <= 0;
   else if (clear) q <= 0;
   else if (en)    q <= d;
endmodule



module mux2 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, 
              input  logic             s, 
              output logic [WIDTH-1:0] y);

  assign y = s ? d1 : d0; 
endmodule



module mux3 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, d2,
              input  logic [1:0]       s, 
              output logic [WIDTH-1:0] y);

  assign #1 y = s[1] ? d2 : (s[0] ? d1 : d0); 
endmodule



module mux4 #(parameter WIDTH = 8)
             (input  logic [WIDTH-1:0] d0, d1, d2, d3,
              input  logic [1:0]       s, 
              output logic [WIDTH-1:0] y);

   always_comb
      case(s)
         2'b00: y <= d0;
         2'b01: y <= d1;
         2'b10: y <= d2;
         2'b11: y <= d3;
      endcase
endmodule



module alu(input  logic [31:0] a, b,
           input  logic [2:0]  alucontrol,
           output logic [31:0] result
           );

  logic [31:0] bout, sum;

  assign bout = alucontrol[2] ? ~b : b;
  assign sum = a + bout + alucontrol[2];

  always_comb
    case (alucontrol[1:0])
      2'b00: result = a & bout;
      2'b01: result = a | bout;
      2'b10: result = sum;
      2'b11: result = sum[31];
    endcase

  assign zero = (result == 32'b0);
endmodule



module eqcmp(input  logic [31:0] a, b,
			 output logic y);
			 
	assign y = (a == b);
	
endmodule



module neqcmp(input  logic [31:0] a, b,
			 output logic y);
			 
	assign y = (a != b);
	
endmodule



module imem(input   logic  [5:0]  a,
            output  logic [31:0] rd);
 logic  [31:0] RAM[63:0];
 initial
   begin
     $readmemh("negs1.dat",RAM);
   end
 assign rd = RAM[a]; // word aligned
endmodule



module dmem(input   logic         clk, we,
            input   logic  [31:0] a, wd,
            output  logic [31:0] rd);
 logic  [31:0] RAM[63:0];
 
 assign rd = RAM[a[31:2]]; // word aligned
 always_ff @(posedge clk)
   if (we)
     RAM[a[31:2]] <= wd;
endmodule



module top(input  logic        clk, reset, 
           output logic [31:0] writedata, dataadr, 
           output logic        memwrite);

  logic [31:0] pc, instr, readdata,fpuout;
  
  // instantiate processor and memories
   mips  mips(       clk, reset, pc,instr,
             memwrite,
             dataadr,fpuout ,writedata,
             readdata);
  imem imem(pc[7:2], instr);
  dmem dmem(clk, memwrite, dataadr, writedata, readdata);
endmodule



module testbench();
 logic         clk;
 logic         reset;
 logic [31:0] writedata, dataadr;
 logic memwrite;
 // instantiate device to be tested
 top dut(clk, reset, writedata, dataadr, memwrite);
 
 // initialize test
 initial
   begin
     reset <= 1; # 22; reset <= 0;
   end
 // generate clock to sequence tests
 always
   begin
     clk <= 1; # 5; clk <= 0; # 5;
   end
 // check results
 always@(negedge clk)
   begin
     if(memwrite) begin
       if(dataadr === 84 & writedata === 7) begin
         $display("Simulation succeeded");
         $stop;
       end else if (dataadr !== 80) begin
         $display("Simulation failed");
         $stop;
       end
     end
   end
endmodule


module testbench_fpu_lw_sw();

logic clk;
logic reset;
logic [31:0] writedata, dataadr;
logic memwrite;
// instantiate device to be tested
top dut (clk, reset, writedata, dataadr, memwrite);

// initialize test
initial
    begin
    reset <= 1; # 22; reset <= 0;
    end

// generate clock to sequence tests
always
    begin
    clk <= 1; # 5; clk <= 0; # 5;
    end

// check results
always @(negedge clk)
    begin
        if (memwrite) begin
                $display("dataadr  %d , writedata %d \n ",dataadr,writedata);
                 end
            if (memwrite==1)
            begin

          if  ( dataadr === 84 & writedata === (2147483653)) begin
        	
        	$display("ssuceed");
         	 $stop;
            end
        end
            end 
            		


endmodule

























