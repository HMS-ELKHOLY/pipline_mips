module mips(input  logic        clk, reset,
            output logic [31:0] pcF,
            input  logic [31:0] instrF,
            output logic [1:0]  memwriteM,
            output logic [31:0] aluoutM,final_writedata,
            input  logic [31:0] readdataM);
			
 logic [5:0]  opD, functD;
 logic        regdstE, alusrcE, 
             pcsrcD,
             memtoregE, memtoregM, memtoregW, 
             regwriteE, regwriteM, regwriteW;
 logic [4:0]  alucontrolE;
 logic        flushE, equalD, nequalD, lezD, gtzD, selD;
 logic [2:0]  load_selW;
 logic float_memtoregE,
       float_memtoregM,
       float_memtoregW,
       float_regwriteE,
       float_regwriteM,
       float_regwriteW,
       float_regdstE,
       fpusrcE,
       float_memwriteM;
 
 logic [3:0]  fpucontrolE;
 controller c(clk, reset, opD, functD, selD, flushE, 
              equalD, nequalD, lezD, gtzD, memtoregE, memtoregM, 
              memtoregW, memwriteM, pcsrcD, 
              branchD, bneD, blezD, bgtzD, alusrcE, regdstE, regwriteE,
              regwriteM, regwriteW, jumpD, extendD, luiD, jrD, linkD, linkE,
              alucontrolE, load_selW,float_memtoregE,
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
		logic [31:0] writedataM,fpuoutM;	  
 datapath dp(clk, reset, memtoregE, memtoregM, 
             memtoregW, pcsrcD, branchD, bneD, blezD, bgtzD,
             alusrcE, regdstE, regwriteE, 
             regwriteM, regwriteW, jumpD, extendD, luiD, jrD, linkD, linkE,
             alucontrolE,
             equalD, nequalD, lezD, gtzD, pcF, instrF,
             aluoutM, writedataM, readdataM,
             opD, functD, selD, flushE, load_selW,
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
           fpuoutM     ,
         final_writedata
         ,float_memwriteM
 
             );
endmodule



module controller(input   logic        clk, reset,
                  input   logic  [5:0] opD, functD,
				          input   logic        selD,
                  input   logic        flushE, equalD, nequalD, lezD, gtzD,
                  output  logic        memtoregE, memtoregM, 
                  output  logic        memtoregW,
                  output  logic [1:0]  memwriteM,
                  output  logic        pcsrcD, branchD, bneD, blezD, bgtzD, alusrcE,
                  output  logic        regdstE, regwriteE, 
                  output  logic        regwriteM, regwriteW,
                  output  logic        jumpD, extendD, luiD, jrD, linkD, linkE,
                  output  logic [4:0]  alucontrolE,
		        		  output  logic [2:0]  load_selW,
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
				  
 logic [2:0] aluopD;logic [3:0] fpucontrolD;
 logic       memtoregD, alusrcD;
 logic [1:0] memwriteD, memwriteE;
 logic       regdstD, regwriteD;
 logic [4:0] alucontrolD;
 logic       jalrD;
 logic [2:0] load_selD, load_selE, load_selM;
  logic [2:0] fpuopD;
  logic    float_memtoregD,
           fpusrcD,
           float_regdstD,
           float_regwriteD,
           float_memwriteD, 
           float_memwriteE 
  ;
 maindec md(opD, memtoregD, memwriteD, branchD, bneD, blezD, bgtzD,
            alusrcD, regdstD, regwriteD, jumpD, extendD, luiD,
            aluopD,

            float_memtoregD,
            fpusrcD,
            float_regdstD,
            float_regwriteD,
            float_memwriteD
      
            );
 fpudec  fpudecoder(functD, fpucontrolD);
 aludec  ad(functD, selD, aluopD, alucontrolD);
 
 assign pcsrcD = (branchD & equalD) | (bneD & nequalD) | (blezD & lezD) | (bgtzD & gtzD);
 assign jrD = (opD == 6'b0) & (functD[5:1] == 5'b100);
 
 assign jalrD = jrD & functD[0];
 assign linkD = (opD == 6'b11) | jalrD;
 
 assign load_selD = opD[2:0];
 
 // pipeline registers
 floprc #(24) regE(clk, reset, flushE,
                 {memtoregD, memwriteD, alusrcD, 
                  regdstD, regwriteD, alucontrolD,linkD, load_selD,float_memtoregD, fpusrcD, 
                  float_regdstD, float_regwriteD, fpucontrolD,float_memwriteD}, 
                 {memtoregE, memwriteE, alusrcE, 
                  regdstE, regwriteE,  alucontrolE,linkE, load_selE, 
                  float_memtoregE, fpusrcE, 
                  float_regdstE, float_regwriteE,  fpucontrolE,float_memwriteE});
 flopr #(10) regM(clk, reset, 
                 {memtoregE, memwriteE, regwriteE, load_selE,float_memtoregE, float_regwriteE,float_memwriteE},
                 {memtoregM, memwriteM, regwriteM, load_selM,float_memtoregM, float_regwriteM,float_memwriteM});
 flopr #(7) regW(clk, reset, 
                 {memtoregM, regwriteM, load_selM,float_memtoregM, float_regwriteM},
                 {memtoregW, regwriteW, load_selW,float_memtoregW , float_regwriteW});
endmodule



 

module maindec(input   logic [5:0] op,
               output  logic       memtoreg,
               output  logic [1:0] memwrite,
               output  logic       branch, bne, blez, bgtz, alusrc,
               output  logic       regdst, regwrite,
               output  logic       jump, extend, lui,
               output  logic [2:0] aluop,   output  logic float_memtoreg,
         output  logic fpusrc,
         output  logic float_regdst,
         output  logic float_regwrite,
         float_memwrite
);

 logic [20:0] controls;
 assign {regwrite, regdst, alusrc,
         branch, bne, blez, bgtz, memwrite,
         memtoreg, jump, extend, lui, aluop,float_memtoreg,fpusrc,float_regdst,float_regwrite,float_memwrite} = controls;

 always_comb
   case(op)
     6'b000000: controls <= 21'b1100000_00_0000_111___00000; //Rtyp
     6'b100000: controls <= 21'b1010000_00_1010_000___00000; //LB
	   6'b100001: controls <= 21'b1010000_00_1010_000___00000; //LH
	   6'b100011: controls <= 21'b1010000_00_1010_000___00000; //LW
     6'b100100: controls <= 21'b1010000_00_1010_000___00000; //LBU
	   6'b100101: controls <= 21'b1010000_00_1010_000___00000; //LHU
	   6'b101000: controls <= 21'b0010000_11_0010_000___00000; //SB
	   6'b101001: controls <= 21'b0010000_10_0010_000___00000; //SH
	   6'b101011: controls <= 21'b0010000_01_0010_000___00000; //SW
     6'b000100: controls <= 21'b0001000_00_0000_001___00000; //BEQ
     6'b001000: controls <= 21'b1010000_00_0010_000___00000; //ADDI
     6'b001001: controls <= 21'b1010000_00_0010_000___00000; //ADDIU
	   6'b000010: controls <= 21'b0000000_00_0100_000___00000; //J
	   6'b000101: controls <= 21'b0000100_00_0000_001___00000; //BNE
     6'b001101: controls <= 21'b1010000_00_0000_010___00000; //ORI
	   6'b000011: controls <= 21'b1000000_00_0100_000___00000; //JAL
	   6'b000110: controls <= 21'b0000010_00_0000_001___00000; //BLEZ
	   6'b000111: controls <= 21'b0000001_00_0000_001___00000; //BGTZ
	   6'b001111: controls <= 21'b1010000_00_0001_000___00000; //LUI
	   6'b001100: controls <= 21'b1010000_00_0000_011___00000; //ANDI
	   6'b001110: controls <= 21'b1010000_00_0000_100___00000; //XORI
	   6'b001010: controls <= 21'b1010000_00_0010_101___00000; //SLTI
	   6'b001011: controls <= 21'b1010000_00_0010_110___00000; //SLTIU
     6'b010001: controls <= 21'b0000000_00_0000_000___00110; //fRtyp//fpusrc ??
     6'b110001: controls <= 21'b0010000_00_1010_000___11010; //flw  /////??
     6'b111001: controls <= 21'b0010000_01_0010_000___01001; //fst   ///??
	   default:   controls <= 21'bxxxxxxx_xx_xxxx_xxx___xxxxx; //???
   endcase
endmodule



module aludec(input   logic   [5:0] funct,
			  input   logic         selD,
              input   logic   [2:0] aluop,
              output  logic   [4:0] alucontrol);

 always_comb
   case(aluop)
     3'b000: alucontrol <= 5'b00010;  //  add (for lw/sw/addi/jal/lui) 
     3'b001: alucontrol <= 5'b00011;  //  sub (for beq/bne)
     3'b010: alucontrol <= 5'b00001;  //  or (for ori)
	 3'b011: alucontrol <= 5'b00000;  //  and (for andi)
	 3'b100: alucontrol <= 5'b00100;  //  xor (for xori)
	 3'b101: alucontrol <= 5'b10101;  //  slt (for slti)
	 3'b110: alucontrol <= 5'b10100;  //  sltu (for sltiu)
	 default: case(funct)          // RTYPE
     6'b100000: alucontrol <= 5'b00010; // ADD
     6'b100010: alucontrol <= 5'b00011; // SUB
     6'b100011: alucontrol <= 5'b00011; // SUBU
		 6'b100100: alucontrol <= 5'b00000; // AND
     6'b100101: alucontrol <= 5'b00001; // OR
		 6'b100110: alucontrol <= 5'b00100; // XOR
		 6'b100111: alucontrol <= 5'b00101; // NOR
     6'b101010: alucontrol <= 5'b10101; // SLT
     6'b101011: alucontrol <= 5'b10100; // SLTU
		 6'b001001: alucontrol <= 5'b00010; // ADD for jalr
         
		 6'b000000: alucontrol <= 5'b00110; //sll
		 6'b000100: alucontrol <= 5'b00111; //sllv
		 6'b000010: alucontrol <= 5'b01000; //srl
		 6'b000110: alucontrol <= 5'b01001; //srlv
		 6'b000011: alucontrol <= 5'b01010; //sra
		 6'b000111: alucontrol <= 5'b01011; //srav
		 
		 6'b011000: alucontrol <= {4'b0110,selD}; //mul //muh
		 6'b011001: alucontrol <= {4'b0111,selD}; //mulu //muhu
		 6'b011010: alucontrol <= {4'b1000,selD}; //div  //mod
		 6'b011011: alucontrol <= {4'b1001,selD}; //divu  //modu
		 default:   alucontrol <= 5'bxxxxx;       // ?????
       endcase
   endcase
endmodule



module datapath(input   logic         clk, reset,
                input   logic         memtoregE, memtoregM, memtoregW, 
                input   logic         pcsrcD, branchD, bneD, blezD, bgtzD,
                input   logic         alusrcE, regdstE,
                input   logic         regwriteE, regwriteM, regwriteW, 
                input   logic         jumpD, extendD, luiD, jrD, linkD, linkE,
                input   logic  [4:0]  alucontrolE,
                output  logic        equalD, nequalD, lezD, gtzD,
                output  logic [31:0] pcF,
                input   logic  [31:0] instrF,
                output  logic [31:0] aluoutM, writedataM,
                input   logic  [31:0] readdataM,
                output  logic [5:0]  opD, functD,
			        	output  logic        selD,
                output  logic        flushE,
		  		input logic  [2:0]  load_selW,
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
);

logic        forwardaD, forwardbD;
logic        forwardaM;
logic [1:0]  forwardaE, forwardbE;
logic        stallF;
logic [4:0]  rsD, rtD, rdD, rsE, rtE, rdE;
logic [4:0]  writeregE, writereg1E, writeregM, writeregW;
logic        flushD;
logic [31:0] pcnextFD, pcnextbrFD, pcnextjFD, pcplus4F, pcbranchD;
logic [31:0] signimmD, zeroimmD, imm1D, immD, immE, signimmshD;
logic [31:0] srcaD, srca1D, srca2D, srcaE, srca2E;
logic [31:0] srcbD, srcb1D, srcb2D, srcbE, srcb2E, srcb3E;
logic [31:0] pcplus4D, instrD;
logic [31:0] aluoutE, aluoutW;
logic [31:0] readdataW, resultW;
logic [4:0] shamtD, shamtE;
logic [31:0] writedata1M;
logic [31:0] readdata1W;
logic        float_forwardaD, float_forwardbD;
logic [1:0]  float_forwardaE, float_forwardbE;
logic [4:0]  float_ftD, float_fsD, float_fdD, float_ftE, float_fsE, float_fdE;
logic [4:0]  float_writeregE, float_writeregM, float_writeregW;
logic [31:0] float_srcaD,  float_srcaE,  float_srca2E ; 
logic [31:0] float_srcbD, float_srcb2D, float_srcbE, float_srcb2E;
logic [31:0] float_writedataM;
logic [31:0] fpuoutE, fpuoutW,float_resultW;
 
 // hazard detection
 hazard    h(rsD, rtD, rsE, rtE, writeregE, writeregM, 
             writeregW,regwriteE, regwriteM, regwriteW, 
             memtoregE, memtoregM, memtoregW, branchD, bneD, blezD, bgtzD, jrD, linkE,
             forwardaD, forwardbD, forwardaM, forwardaE, 
             forwardbE,
             stallF, stallD, flushE,
      
      float_ftD, float_fsD, float_ftE, float_fsE,
      float_writeregE, float_writeregM, 
      float_writeregW,float_regwriteE, float_regwriteM, float_regwriteW, 
      float_memtoregE, float_memtoregM,
      float_forwardaD, float_forwardbD, float_forwardaE, 
      float_forwardbE);

 // next PC logic (operates in fetch and decode)
mux2 #(32)  pcbrmux(pcplus4F, pcbranchD, pcsrcD, 
                     pcnextbrFD);
mux2 #(32)  pcjumpmux(pcnextbrFD,{pcplus4D[31:28], 
                   instrD[25:0], 2'b00}, 
                   jumpD, pcnextjFD);
				   
mux2 #(32)  pcjrmux(pcnextjFD, srca2D, jrD, pcnextFD);

 // register file (operates in decode and writeback)
regfile     rf(clk, regwriteW, rsD, rtD, writeregW,
                resultW, srca1D, srcb1D);
regfile     float_rf(clk, float_regwriteW, float_fsD, float_ftD, float_writeregW,
                float_resultW, float_srcaD, float_srcbD);
// Fetch stage logic
 flopenr #(32) pcreg(clk, reset, ~stallF, 
                     pcnextFD, pcF);
 adder       pcadd1(pcF, 32'b100, pcplus4F);
assign float_ftD = instrD[20:16];
 assign float_fsD = instrD[15:11];
 assign float_fdD = instrD[10:6];
 
// Decode stage 
 flopenr  #(32) r1D(clk, reset, ~stallD, pcplus4F,pcplus4D);
 flopenrc #(32) r2D(clk, reset, ~stallD, flushD, instrF, instrD);
 signext        se(instrD[15:0], signimmD);
 zeroext        ze(instrD[15:0], zeroimmD);
 mux2 #(32)     ext(zeroimmD, signimmD, extendD, imm1D);
 mux2 #(32)     lui(imm1D, {instrD[15:0],16'b0}, luiD, immD);
 
 sl2         immsh(signimmD, signimmshD);
 adder       pcadd2(pcplus4D, signimmshD, pcbranchD);
 mux2 #(32)  forwardadmux(srca1D, aluoutM, forwardaD,srca2D);
 mux2 #(32)  forwardbdmux(srcb1D, aluoutM, forwardbD,srcb2D);
 eqcmp       comp(srca2D, srcb2D, equalD);
 neqcmp      ncom(srca2D, srcb2D, nequalD);
 lez         lez(srca2D, lezD);
 gtz         gtz(srca2D, gtzD);
 
 mux2 #(64) linkmux({srca1D, srcb1D},{pcplus4D,32'b0},linkD,{srcaD,srcbD});
 
 assign opD = instrD[31:26];
 assign functD = instrD[5:0];
 assign rsD = instrD[25:21];
 assign rtD = instrD[20:16];
 assign rdD = instrD[15:11];
 assign selD = instrD[6];
 assign shamtD = instrD[10:6];
 assign flushD = (pcsrcD & ~stallD) | jumpD | (jrD & ~stallD);
 
 // Execute stage 
 floprc #(32) r1E(clk, reset, flushE, srcaD, srcaE);
 floprc #(32) r2E(clk, reset, flushE, srcbD, srcbE);
 floprc #(32) r3E(clk, reset, flushE, immD, immE);
 floprc #(5)  r4E(clk, reset, flushE, rsD, rsE);
 floprc #(5)  r5E(clk, reset, flushE, rtD, rtE);
 floprc #(5)  r6E(clk, reset, flushE, rdD, rdE);
 floprc #(5)  r7E(clk, reset, flushE, shamtD, shamtE);
 mux3 #(32)  forwardaemux(srcaE, resultW, aluoutM, forwardaE, srca2E);
 mux3 #(32)  forwardbemux(srcbE, resultW, aluoutM, forwardbE, srcb2E);
 mux2 #(32)  srcbmux(srcb2E, immE, alusrcE, srcb3E);
 alu         alu(srca2E, srcb3E, shamtE, alucontrolE, aluoutE);
 fpu   fpu( clk,
             fpucontrolE ,       
             float_srca2E,float_srcb2E, 
            fpuoutE);
 mux2 #(5)   wrmux(rtE, rdE, regdstE, writereg1E);
 mux2 #(5)   lnmux(writereg1E, 5'b11111, linkE, writeregE);
// logic[31:0] writedataM;
 // Memory stage
 flopr #(32) r1M(clk, reset, srcb2E, writedata1M);
 mux2  #(32) forwardmux(writedata1M, resultW, forwardaM, writedataM);
 flopr #(32) r2M(clk, reset, aluoutE, aluoutM);
 flopr #(5)  r3M(clk, reset, writeregE, writeregM);
 
 // Writeback stage
 flopr #(32) r1W(clk, reset, aluoutM, aluoutW);
 flopr #(32) r2W(clk, reset, readdataM, readdata1W);
 flopr #(5)  r3W(clk, reset, writeregM, writeregW);
 memout      mo(readdata1W, load_selW, readdataW);
 mux2 #(32)  resmux(aluoutW, readdataW, memtoregW, resultW);




 floprc #(32) float_r1E(clk, reset, flushE, float_srcaD, float_srcaE);
 floprc #(32) float_r2E(clk, reset, flushE, float_srcbD, float_srcbE);
 floprc #(5)  float_r4E(clk, reset, flushE, float_ftD, float_ftE);
 floprc #(5)  float_r5E(clk, reset, flushE, float_fsD, float_fsE);
 floprc #(5)  float_r6E(clk, reset, flushE, float_fdD, float_fdE);

 mux3 #(32)  float_forwardaemux(float_srcaE, float_resultW, fpuoutM, float_forwardaE, float_srca2E);
 mux3 #(32)  float_forwardbemux(float_srcbE, float_resultW, fpuoutM, float_forwardbE, float_srcb2E);


mux2 #(5)   float_wrmux(float_ftE, float_fdE, float_regdstE, float_writeregE);



 flopr #(32) floaT_r2M(clk, reset, fpuoutE, fpuoutM);
 flopr #(5)  float_r3M(clk, reset, float_writeregE, float_writeregM);
 flopr #(32) float_r4M(clk, reset, float_srcb2E, float_writedataM);
 
 mux2 #(32) finalwrite(writedataM,float_writedataM,float_memwriteM,final_writedata);

flopr #(32) float_r1W(clk, reset, fpuoutM, fpuoutW);
//flopr #(32) float_r2W(clk, reset, float_readdataM, float_readdataW);
flopr #(5)  float_r3W(clk, reset, float_writeregM, float_writeregW);
mux2 #(32)  float_resmux(fpuoutW, readdataW, float_memtoregW, float_resultW);
 
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
   // $display("wa :%d   WD%d",wa3,wd3);
  
    if (we3) rf[wa3] <= wd3;	
end
  assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0;

endmodule



module hazard(input   logic  [4:0] rsD, rtD, rsE, rtE, 
              input   logic  [4:0] writeregE, writeregM, writeregW,
              input   logic        regwriteE, regwriteM, regwriteW,
              input   logic        memtoregE, memtoregM, memtoregW, branchD, bneD, blezD, bgtzD, jrD, linkE,
              output  logic           forwardaD, forwardbD, forwardaM,
              output  logic [1:0] forwardaE, forwardbE,
              output  logic       stallF, stallD, flushE,
              input   logic  [4:0] float_ftD, float_fsD, float_ftE, float_fsE,
              input   logic  [4:0] float_writeregE, float_writeregM,float_writeregW,
              input   logic        float_regwriteE, float_regwriteM, float_regwriteW, 
              input   logic        float_memtoregE, float_memtoregM,
              output  logic        float_forwardaD, float_forwardbD, 
              output  logic [1:0]  float_forwardaE,float_forwardbE);
			  
 logic lwstallD, branchstallD, jrstall, bzstall;
 
 // forwarding sources to D stage (branch equality)
 assign forwardaD = (rsD !=0 & rsD == writeregM & regwriteM);
 assign forwardbD = (rtD !=0 & rtD == writeregM & regwriteM);
 
 // forwarding sources to E stage (ALU)
 always_comb
   begin
     forwardaE = 2'b00; forwardbE = 2'b00;
     if ((rsE != 0) & ~linkE)
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
			
assign  #1 jrstall = (jrD) & (regwriteE & (writeregE == rsD) | memtoregM & (writeregM == rsD));

assign  #1 bzstall = (blezD | bgtzD) & (regwriteE & (writeregE == rsD) | memtoregM & (writeregM == rsD));

 assign #1 stallD = lwstallD | branchstallD | jrstall | bzstall;
 assign #1 stallF = stallD;
   // stalling D stalls all previous stages
 assign #1 flushE = stallD; 
   // stalling D flushes next stage
 // Note: not necessary to stall D stage on store 
 //       if source comes from load;
 //       instead, another bypass network could 
 //       be added from W to M
 
 assign forwardaM = memtoregW & (writeregM == writeregW);




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
   if      (reset) q <= #1 0;
   else if (clear) q <= #1 0;
   else            q <= #1 d;
endmodule



module flopenrc #(parameter WIDTH = 8)
                (input   logic     clk, reset,
                 input   logic      en, clear,
                 input   logic  [WIDTH-1:0] d, 
                 output  logic [WIDTH-1:0] q);
 always_ff @(posedge clk, posedge reset)
   if      (reset) q <= #1 0;
   else if (clear) q <= #1 0;
   else if (en)    q <= #1 d;
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
        input logic [4:0] shamt,
        input  logic [4:0]  alucontrol,
        output logic [31:0] result);

logic [31:0] bout, sum;
logic [4:0] shift;


assign bout = alucontrol[0] ? ~b : b; //for subtraction set alucontrol[0] to 1
assign sum = a + bout + alucontrol[0];

assign shift = alucontrol[0] ? a[4:0] : shamt; //for shift varaible alucontrol[0] = 1 

logic [63:0] mult = $signed(a) * $signed(b);
logic [63:0] multu = a * b;

logic [31:0] div  = $signed(a) / $signed(b);
logic [31:0] divu = a / b;

logic [31:0] mod  = $signed(a) % $signed(b);
logic [31:0] modu = a % b;

logic [31:0] mul;
assign mul = alucontrol[0] ? mult[63:32] : mult[31:0];

logic [31:0] mulu;
assign mulu = alucontrol[0] ? multu[63:32] : multu[31:0];

logic [31:0] divmod;
assign divmod = alucontrol[0] ? mod : div;

logic [31:0] divmodu;
assign divmodu = alucontrol[0] ? modu : divu;

always_comb
 case (alucontrol) inside
   5'b00000: result = a & b; //and 00000
   5'b00001: result = a | b; //or  00001
   5'b0001?: result = sum;      //sub 00011  //add 00010
   
   5'b00100: result = a ^ b;    //xor 
   5'b00101: result = ~(a | b);    //nor
   
   5'b0011?: result = b << shift;  //sll 00110  //sllv 00111
   5'b0100?: result = b >> shift;  //srl 01000  //srlv 01001
   5'b0101?: result = $signed(b) >>> shift;  //sra 01010  //srav 01011
   
   5'b0110?: result = mul;
   5'b0111?: result = mulu;
   5'b1000?: result = divmod;
   5'b1001?: result = divmodu;
   
   5'b10100: result = (a < b);  //sltu
   5'b10101: result = sum[31];  //slt 
   default:  result = 32'hxxxxxxxx; // ???
 endcase

endmodule



module eqcmp(input  logic [31:0] a, b,
			 output logic y);
			 
	assign y = (a == b);
	
endmodule



module neqcmp(input  logic [31:0] a, b,
			 output logic y);
			 
	assign y = (a != b);
	
endmodule



module lez(input  logic [31:0] a,
			 output logic y);
			 
	assign y = ($signed(a) <= $signed(32'b0));
	
endmodule



module gtz(input  logic [31:0] a,
			 output logic y);
			 
	assign y = ($signed(a) > $signed(32'b0));
	
endmodule



module memout(input logic [31:0] rd_temp,
              input logic [2:0]  load_sel,
              output logic [31:0] rd);
			  
always_comb
case (load_sel)
   3'b000: rd <= {{24{rd_temp[7]}}, rd_temp[7:0]};   //LB
   3'b001: rd <= {{16{rd_temp[15]}}, rd_temp[15:0]}; //LH
   3'b011: rd <= rd_temp;                            //LW
   3'b100: rd <= {24'b0, rd_temp[7:0]};              //LBU
   3'b101: rd <= {16'b0, rd_temp[15:0]};             //LHU
endcase
			  
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



module dmem(input   logic        clk, 
            input   logic [1:0]  we,
            input   logic [31:0] a, wd,
            output  logic [31:0] rd);
 logic  [31:0] RAM[63:0];
 
 assign rd = RAM[a[31:2]]; // word aligned
 always_ff @(posedge clk)begin
    if ( we == 2'b01 ) begin
      RAM[a[31:2]] <= wd;
    end 
    else if ( we == 2'b10 ) begin
      // {a[1],4'b0000} uses the second LSB as an indeicator to the upper 
      // or lower word starting point
      // which is an intuitive approuch to reach the half word
      RAM[a[31:2]][ {a[1],4'b0000} +: 16] <= wd[15:0]; // sh
    end
    else if (we == 2'b11) begin
      // {a[1:0],3'b000} uses the first and second LSB as an indeicator to the  
      // specified byte starting point
      // which is an intuitive approuch to reach the byte
      RAM[a[31:2]][ {a[1:0],3'b000} +: 8] <= wd[7:0]; // sb
    end
  end
   
endmodule



module top(input  logic        clk, reset, 
           output logic [31:0] writedata, dataadr, 
           output logic [1:0]      memwrite);

  logic [31:0] pc, instr, readdata;
  
  // instantiate processor and memories
  mips mips(clk, reset, pc, instr, memwrite, dataadr, 
            writedata, readdata);
  imem imem(pc[7:2], instr);
  dmem dmem(clk, memwrite, dataadr, writedata, readdata);
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

module testbench();
 logic         clk;
 logic         reset;
 logic [31:0] writedata, dataadr;
 logic [1:0]  memwrite;
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
logic [1:0] memwrite;
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
               
          if  ( dataadr === 84 & writedata === 32'd2147483653) begin
          
          $display("ssuceed");
           $stop;
            end
            else if (dataadr!=80)
            begin
          $display("fail");
           $stop;
            end
            end
            end 
                

endmodule


