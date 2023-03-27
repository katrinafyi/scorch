; ModuleID = 'df'
source_filename = "<string>"

@decode_table_0 = global <16 x ptr> [ptr blockaddress(@f, %17), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %20), ptr false]
@decode_table_1 = global <16 x ptr> [ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %13), ptr false]
@decode_table_2 = global <16 x ptr> [ptr blockaddress(@f, %9), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_3 = global <16 x ptr> [ptr blockaddress(@f, %56), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_4 = global <16 x ptr> [ptr blockaddress(@f, %80), ptr blockaddress(@f, %87), ptr blockaddress(@f, %94), ptr blockaddress(@f, %101), ptr blockaddress(@f, %108), ptr blockaddress(@f, %115), ptr blockaddress(@f, %122), ptr blockaddress(@f, %129), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %136), ptr false]
@decode_table_5 = global <16 x ptr> [ptr blockaddress(@f, %148), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_6 = global <16 x ptr> [ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %184), ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_7 = global <16 x ptr> [ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %195), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_8 = global <16 x ptr> [ptr false, ptr blockaddress(@f, %177), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %188), ptr false]
@decode_table_9 = global <16 x ptr> [ptr blockaddress(@f, %209), ptr blockaddress(@f, %216), ptr blockaddress(@f, %223), ptr blockaddress(@f, %230), ptr blockaddress(@f, %237), ptr blockaddress(@f, %244), ptr blockaddress(@f, %251), ptr blockaddress(@f, %258), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %265), ptr false]
@decode_table_10 = global <16 x ptr> [ptr blockaddress(@f, %5), ptr blockaddress(@f, %30), ptr blockaddress(@f, %34), ptr blockaddress(@f, %40), ptr blockaddress(@f, %46), ptr blockaddress(@f, %48), ptr blockaddress(@f, %64), ptr blockaddress(@f, %70), ptr blockaddress(@f, %72), ptr blockaddress(@f, %140), ptr blockaddress(@f, %154), ptr blockaddress(@f, %158), ptr blockaddress(@f, %164), ptr blockaddress(@f, %172), ptr blockaddress(@f, %174), ptr blockaddress(@f, %201)]

define void @f(i10 %param_0) {
  br label %270

1:                                                ; preds = %270
  %2 = lshr exact i10 %param_0, 3
  %3 = trunc i10 %2 to i1
  br label %269

4:                                                ; No predecessors!
  br label %27

5:                                                ; preds = %269
  %6 = lshr exact i10 %param_0, 2
  %7 = trunc i10 %6 to i1
  br label %26

8:                                                ; No predecessors!
  br label %25

9:                                                ; preds = %26
  %10 = lshr exact i10 %param_0, 1
  %11 = trunc i10 %10 to i1
  br label %24

12:                                               ; No predecessors!
  br label %23

13:                                               ; preds = %24
  %14 = trunc i10 %param_0 to i1
  br label %22

15:                                               ; No predecessors!
  br label %18

16:                                               ; No predecessors!
  ret void

17:                                               ; preds = %22
  ret void

18:                                               ; preds = %15
  br label %21

19:                                               ; No predecessors!
  ret void

20:                                               ; preds = %22
  ret void

21:                                               ; preds = %18
  ret void

22:                                               ; preds = %13
  indirectbr i1 %14, [label %17, label %20]

23:                                               ; preds = %12
  ret void

24:                                               ; preds = %9
  indirectbr i1 %11, [label %13]

25:                                               ; preds = %8
  ret void

26:                                               ; preds = %5
  indirectbr i1 %7, [label %9]

27:                                               ; preds = %4
  br label %31

28:                                               ; No predecessors!
  %29 = trunc i10 %param_0 to i3
  ret void

30:                                               ; preds = %269
  ret void

31:                                               ; preds = %27
  br label %35

32:                                               ; No predecessors!
  %33 = trunc i10 %param_0 to i3
  ret void

34:                                               ; preds = %269
  ret void

35:                                               ; preds = %31
  br label %41

36:                                               ; No predecessors!
  %37 = lshr exact i10 %param_0, i8 2
  %38 = trunc i10 %37 to i1
  %39 = trunc i10 %param_0 to i2
  ret void

40:                                               ; preds = %269
  ret void

41:                                               ; preds = %35
  br label %47

42:                                               ; No predecessors!
  %43 = lshr exact i10 %param_0, i8 2
  %44 = trunc i10 %43 to i1
  %45 = trunc i10 %param_0 to i2
  ret void

46:                                               ; preds = %269
  ret void

47:                                               ; preds = %41
  br label %59

48:                                               ; preds = %269
  %49 = trunc i10 %param_0 to i1
  br label %58

50:                                               ; No predecessors!
  br label %57

51:                                               ; No predecessors!
  %52 = lshr exact i10 %param_0, i8 2
  %53 = trunc i10 %52 to i1
  %54 = lshr exact i10 %param_0, i9 1
  %55 = trunc i10 %54 to i1
  ret void

56:                                               ; preds = %58
  ret void

57:                                               ; preds = %50
  ret void

58:                                               ; preds = %48
  indirectbr i1 %49, [label %56]

59:                                               ; preds = %47
  br label %65

60:                                               ; No predecessors!
  %61 = lshr exact i10 %param_0, i8 2
  %62 = trunc i10 %61 to i1
  %63 = trunc i10 %param_0 to i2
  ret void

64:                                               ; preds = %269
  ret void

65:                                               ; preds = %59
  br label %71

66:                                               ; No predecessors!
  %67 = lshr exact i10 %param_0, i8 2
  %68 = trunc i10 %67 to i1
  %69 = trunc i10 %param_0 to i2
  ret void

70:                                               ; preds = %269
  ret void

71:                                               ; preds = %65
  br label %139

72:                                               ; preds = %269
  %73 = trunc i10 %param_0 to i1
  br label %138

74:                                               ; No predecessors!
  br label %81

75:                                               ; No predecessors!
  %76 = lshr exact i10 %param_0, i8 2
  %77 = trunc i10 %76 to i1
  %78 = lshr exact i10 %param_0, i9 1
  %79 = trunc i10 %78 to i1
  ret void

80:                                               ; preds = %138
  ret void

81:                                               ; preds = %74
  br label %88

82:                                               ; No predecessors!
  %83 = lshr exact i10 %param_0, i8 2
  %84 = trunc i10 %83 to i1
  %85 = lshr exact i10 %param_0, i9 1
  %86 = trunc i10 %85 to i1
  ret void

87:                                               ; preds = %138
  ret void

88:                                               ; preds = %81
  br label %95

89:                                               ; No predecessors!
  %90 = lshr exact i10 %param_0, i8 2
  %91 = trunc i10 %90 to i1
  %92 = lshr exact i10 %param_0, i9 1
  %93 = trunc i10 %92 to i1
  ret void

94:                                               ; preds = %138
  ret void

95:                                               ; preds = %88
  br label %102

96:                                               ; No predecessors!
  %97 = lshr exact i10 %param_0, i8 2
  %98 = trunc i10 %97 to i1
  %99 = lshr exact i10 %param_0, i9 1
  %100 = trunc i10 %99 to i1
  ret void

101:                                              ; preds = %138
  ret void

102:                                              ; preds = %95
  br label %109

103:                                              ; No predecessors!
  %104 = lshr exact i10 %param_0, i8 2
  %105 = trunc i10 %104 to i1
  %106 = lshr exact i10 %param_0, i9 1
  %107 = trunc i10 %106 to i1
  ret void

108:                                              ; preds = %138
  ret void

109:                                              ; preds = %102
  br label %116

110:                                              ; No predecessors!
  %111 = lshr exact i10 %param_0, i8 2
  %112 = trunc i10 %111 to i1
  %113 = lshr exact i10 %param_0, i9 1
  %114 = trunc i10 %113 to i1
  ret void

115:                                              ; preds = %138
  ret void

116:                                              ; preds = %109
  br label %123

117:                                              ; No predecessors!
  %118 = lshr exact i10 %param_0, i8 2
  %119 = trunc i10 %118 to i1
  %120 = lshr exact i10 %param_0, i9 1
  %121 = trunc i10 %120 to i1
  ret void

122:                                              ; preds = %138
  ret void

123:                                              ; preds = %116
  br label %130

124:                                              ; No predecessors!
  %125 = lshr exact i10 %param_0, i8 2
  %126 = trunc i10 %125 to i1
  %127 = lshr exact i10 %param_0, i9 1
  %128 = trunc i10 %127 to i1
  ret void

129:                                              ; preds = %138
  ret void

130:                                              ; preds = %123
  br label %137

131:                                              ; No predecessors!
  %132 = lshr exact i10 %param_0, i8 2
  %133 = trunc i10 %132 to i1
  %134 = lshr exact i10 %param_0, i9 1
  %135 = trunc i10 %134 to i1
  ret void

136:                                              ; preds = %138
  ret void

137:                                              ; preds = %130
  ret void

138:                                              ; preds = %72
  indirectbr i1 %73, [label %80, label %87, label %94, label %101, label %108, label %115, label %122, label %129, label %136]

139:                                              ; preds = %71
  br label %151

140:                                              ; preds = %269
  %141 = trunc i10 %param_0 to i1
  br label %150

142:                                              ; No predecessors!
  br label %149

143:                                              ; No predecessors!
  %144 = lshr exact i10 %param_0, i8 2
  %145 = trunc i10 %144 to i1
  %146 = lshr exact i10 %param_0, i9 1
  %147 = trunc i10 %146 to i1
  ret void

148:                                              ; preds = %150
  ret void

149:                                              ; preds = %142
  ret void

150:                                              ; preds = %140
  indirectbr i1 %141, [label %148]

151:                                              ; preds = %139
  br label %155

152:                                              ; No predecessors!
  %153 = trunc i10 %param_0 to i3
  ret void

154:                                              ; preds = %269
  ret void

155:                                              ; preds = %151
  br label %159

156:                                              ; No predecessors!
  %157 = trunc i10 %param_0 to i3
  ret void

158:                                              ; preds = %269
  ret void

159:                                              ; preds = %155
  br label %165

160:                                              ; No predecessors!
  %161 = lshr exact i10 %param_0, i8 2
  %162 = trunc i10 %161 to i1
  %163 = trunc i10 %param_0 to i2
  ret void

164:                                              ; preds = %269
  ret void

165:                                              ; preds = %159
  br label %173

166:                                              ; No predecessors!
  %167 = lshr exact i10 %param_0, i8 2
  %168 = trunc i10 %167 to i1
  %169 = lshr exact i10 %param_0, i9 1
  %170 = trunc i10 %169 to i1
  %171 = trunc i10 %param_0 to i1
  ret void

172:                                              ; preds = %269
  ret void

173:                                              ; preds = %165
  br label %200

174:                                              ; preds = %269
  %175 = trunc i10 %param_0 to i1
  br label %199

176:                                              ; No predecessors!
  br label %187

177:                                              ; preds = %199
  %178 = lshr exact i10 %param_0, 1
  %179 = trunc i10 %178 to i1
  br label %186

180:                                              ; No predecessors!
  br label %185

181:                                              ; No predecessors!
  %182 = lshr exact i10 %param_0, i8 2
  %183 = trunc i10 %182 to i1
  ret void

184:                                              ; preds = %186
  ret void

185:                                              ; preds = %180
  ret void

186:                                              ; preds = %177
  indirectbr i1 %179, [label %184]

187:                                              ; preds = %176
  br label %198

188:                                              ; preds = %199
  %189 = lshr exact i10 %param_0, 1
  %190 = trunc i10 %189 to i1
  br label %197

191:                                              ; No predecessors!
  br label %196

192:                                              ; No predecessors!
  %193 = lshr exact i10 %param_0, i8 2
  %194 = trunc i10 %193 to i1
  ret void

195:                                              ; preds = %197
  ret void

196:                                              ; preds = %191
  ret void

197:                                              ; preds = %188
  indirectbr i1 %190, [label %195]

198:                                              ; preds = %187
  ret void

199:                                              ; preds = %174
  indirectbr i1 %175, [label %177, label %188]

200:                                              ; preds = %173
  br label %268

201:                                              ; preds = %269
  %202 = trunc i10 %param_0 to i1
  br label %267

203:                                              ; No predecessors!
  br label %210

204:                                              ; No predecessors!
  %205 = lshr exact i10 %param_0, i8 2
  %206 = trunc i10 %205 to i1
  %207 = lshr exact i10 %param_0, i9 1
  %208 = trunc i10 %207 to i1
  ret void

209:                                              ; preds = %267
  ret void

210:                                              ; preds = %203
  br label %217

211:                                              ; No predecessors!
  %212 = lshr exact i10 %param_0, i8 2
  %213 = trunc i10 %212 to i1
  %214 = lshr exact i10 %param_0, i9 1
  %215 = trunc i10 %214 to i1
  ret void

216:                                              ; preds = %267
  ret void

217:                                              ; preds = %210
  br label %224

218:                                              ; No predecessors!
  %219 = lshr exact i10 %param_0, i8 2
  %220 = trunc i10 %219 to i1
  %221 = lshr exact i10 %param_0, i9 1
  %222 = trunc i10 %221 to i1
  ret void

223:                                              ; preds = %267
  ret void

224:                                              ; preds = %217
  br label %231

225:                                              ; No predecessors!
  %226 = lshr exact i10 %param_0, i8 2
  %227 = trunc i10 %226 to i1
  %228 = lshr exact i10 %param_0, i9 1
  %229 = trunc i10 %228 to i1
  ret void

230:                                              ; preds = %267
  ret void

231:                                              ; preds = %224
  br label %238

232:                                              ; No predecessors!
  %233 = lshr exact i10 %param_0, i8 2
  %234 = trunc i10 %233 to i1
  %235 = lshr exact i10 %param_0, i9 1
  %236 = trunc i10 %235 to i1
  ret void

237:                                              ; preds = %267
  ret void

238:                                              ; preds = %231
  br label %245

239:                                              ; No predecessors!
  %240 = lshr exact i10 %param_0, i8 2
  %241 = trunc i10 %240 to i1
  %242 = lshr exact i10 %param_0, i9 1
  %243 = trunc i10 %242 to i1
  ret void

244:                                              ; preds = %267
  ret void

245:                                              ; preds = %238
  br label %252

246:                                              ; No predecessors!
  %247 = lshr exact i10 %param_0, i8 2
  %248 = trunc i10 %247 to i1
  %249 = lshr exact i10 %param_0, i9 1
  %250 = trunc i10 %249 to i1
  ret void

251:                                              ; preds = %267
  ret void

252:                                              ; preds = %245
  br label %259

253:                                              ; No predecessors!
  %254 = lshr exact i10 %param_0, i8 2
  %255 = trunc i10 %254 to i1
  %256 = lshr exact i10 %param_0, i9 1
  %257 = trunc i10 %256 to i1
  ret void

258:                                              ; preds = %267
  ret void

259:                                              ; preds = %252
  br label %266

260:                                              ; No predecessors!
  %261 = lshr exact i10 %param_0, i8 2
  %262 = trunc i10 %261 to i1
  %263 = lshr exact i10 %param_0, i9 1
  %264 = trunc i10 %263 to i1
  ret void

265:                                              ; preds = %267
  ret void

266:                                              ; preds = %259
  ret void

267:                                              ; preds = %201
  indirectbr i1 %202, [label %209, label %216, label %223, label %230, label %237, label %244, label %251, label %258, label %265]

268:                                              ; preds = %200
  ret void

269:                                              ; preds = %1
  indirectbr i1 %3, [label %5, label %30, label %34, label %40, label %46, label %48, label %64, label %70, label %72, label %140, label %154, label %158, label %164, label %172, label %174, label %201]

270:                                              ; preds = %0
  br label %1
}
