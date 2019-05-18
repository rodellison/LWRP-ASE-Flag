// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Rod/ASE-LWRP-Flag"
{
    Properties
    {
		_FlagTexture("Flag Texture", 2D) = "white" {}
		_TextureColor("Texture Color", Color) = (1,1,1,0)
		_TextureRotation("Texture Rotation", Float) = 0
		_FlagNormal("Flag Normal", 2D) = "white" {}
		_FlagNormalScale("Flag Normal Scale", Range( 0 , 5)) = 1
		_XFrequency("XFrequency", Float) = 1
		_Speed("Speed", Float) = 1
		_ZFrequency("ZFrequency", Float) = 1
		_Step("Step", Float) = 0.03
		_ClampFactor("Clamp Factor", Float) = 0.3
		_VerticleNoise("Verticle Noise", Float) = 1
    }


    SubShader
    {
		
        Tags { "RenderPipeline"="LightweightPipeline" "RenderType"="Opaque" "Queue"="Geometry" }

		Cull Off
		HLSLINCLUDE
		#pragma target 4.6
		ENDHLSL
		
        Pass
        {
			
        	Tags { "LightMode"="LightweightForward" }

        	Name "Base"
			Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
            
        	HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x
            

        	// -------------------------------------
            // Lightweight Pipeline keywords
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
            #pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
            #pragma multi_compile _ _SHADOWS_SOFT
            #pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
            
        	// -------------------------------------
            // Unity defined keywords
            #pragma multi_compile _ DIRLIGHTMAP_COMBINED
            #pragma multi_compile _ LIGHTMAP_ON
            #pragma multi_compile_fog

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex vert
        	#pragma fragment frag

        	#define ASE_SRP_VERSION 51300
        	#define _NORMALMAP 1


        	#include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
        	#include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Lighting.hlsl"
        	#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
        	#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
        	#include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"

			float _Step;
			float _XFrequency;
			float _Speed;
			float _ZFrequency;
			float _ClampFactor;
			float _VerticleNoise;
			float4 _TextureColor;
			sampler2D _FlagTexture;
			float4 _FlagTexture_ST;
			float _TextureRotation;
			sampler2D _FlagNormal;
			float _FlagNormalScale;

            struct GraphVertexInput
            {
                float4 vertex : POSITION;
                float3 ase_normal : NORMAL;
                float4 ase_tangent : TANGENT;
                float4 texcoord1 : TEXCOORD1;
				float4 ase_texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct GraphVertexOutput
            {
                float4 clipPos                : SV_POSITION;
                float4 lightmapUVOrVertexSH	  : TEXCOORD0;
        		half4 fogFactorAndVertexLight : TEXCOORD1; // x: fogFactor, yzw: vertex light
            	float4 shadowCoord            : TEXCOORD2;
				float4 tSpace0					: TEXCOORD3;
				float4 tSpace1					: TEXCOORD4;
				float4 tSpace2					: TEXCOORD5;
				float4 ase_texcoord7 : TEXCOORD7;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            	UNITY_VERTEX_OUTPUT_STEREO
            };

			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

            GraphVertexOutput vert (GraphVertexInput v  )
        	{
        		GraphVertexOutput o = (GraphVertexOutput)0;
                UNITY_SETUP_INSTANCE_ID(v);
            	UNITY_TRANSFER_INSTANCE_ID(v, o);
        		UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float myWorldPosX55 = v.vertex.xyz.x;
				float temp_output_34_0 = step( myWorldPosX55 , _Step );
				float mySpeed28 = _Speed;
				float temp_output_26_0 = ( _Time.y * mySpeed28 );
				float myWorldPosZ56 = v.vertex.xyz.z;
				float2 temp_cast_0 = (_VerticleNoise).xx;
				float temp_output_102_0 = ( _Time.y * 0.6 );
				float2 appendResult104 = (float2(temp_output_102_0 , temp_output_102_0));
				float2 uv0106 = v.ase_texcoord * temp_cast_0 + appendResult104;
				float simplePerlin2D107 = snoise( uv0106 );
				float smoothstepResult117 = smoothstep( -1.0 , 1.5 , simplePerlin2D107);
				float3 appendResult60 = (float3(( myWorldPosX55 / 100.0 ) , (-_ClampFactor + (( ( temp_output_34_0 * sin( ( ( _XFrequency * temp_output_26_0 ) + myWorldPosX55 ) ) ) * sin( ( myWorldPosZ56 + ( temp_output_26_0 * _ZFrequency ) ) ) ) - -1.0) * (_ClampFactor - -_ClampFactor) / (1.0 - -1.0)) , ( ( myWorldPosZ56 / 100.0 ) + ( temp_output_34_0 * smoothstepResult117 ) )));
				
				o.ase_texcoord7.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				float3 vertexValue = appendResult60;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal =  v.ase_normal ;

        		// Vertex shader outputs defined by graph
                float3 lwWNormal = TransformObjectToWorldNormal(v.ase_normal);
				float3 lwWorldPos = TransformObjectToWorld(v.vertex.xyz);
				float3 lwWTangent = TransformObjectToWorldDir(v.ase_tangent.xyz);
				float3 lwWBinormal = normalize(cross(lwWNormal, lwWTangent) * v.ase_tangent.w);
				o.tSpace0 = float4(lwWTangent.x, lwWBinormal.x, lwWNormal.x, lwWorldPos.x);
				o.tSpace1 = float4(lwWTangent.y, lwWBinormal.y, lwWNormal.y, lwWorldPos.y);
				o.tSpace2 = float4(lwWTangent.z, lwWBinormal.z, lwWNormal.z, lwWorldPos.z);

                VertexPositionInputs vertexInput = GetVertexPositionInputs(v.vertex.xyz);
                
         		// We either sample GI from lightmap or SH.
        	    // Lightmap UV and vertex SH coefficients use the same interpolator ("float2 lightmapUV" for lightmap or "half3 vertexSH" for SH)
                // see DECLARE_LIGHTMAP_OR_SH macro.
        	    // The following funcions initialize the correct variable with correct data
        	    OUTPUT_LIGHTMAP_UV(v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy);
        	    OUTPUT_SH(lwWNormal, o.lightmapUVOrVertexSH.xyz);

        	    half3 vertexLight = VertexLighting(vertexInput.positionWS, lwWNormal);
        	    half fogFactor = ComputeFogFactor(vertexInput.positionCS.z);
        	    o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
        	    o.clipPos = vertexInput.positionCS;

        	#ifdef _MAIN_LIGHT_SHADOWS
        		o.shadowCoord = GetShadowCoord(vertexInput);
        	#endif
        		return o;
        	}

        	half4 frag (GraphVertexOutput IN  ) : SV_Target
            {
            	UNITY_SETUP_INSTANCE_ID(IN);

        		float3 WorldSpaceNormal = normalize(float3(IN.tSpace0.z,IN.tSpace1.z,IN.tSpace2.z));
				float3 WorldSpaceTangent = float3(IN.tSpace0.x,IN.tSpace1.x,IN.tSpace2.x);
				float3 WorldSpaceBiTangent = float3(IN.tSpace0.y,IN.tSpace1.y,IN.tSpace2.y);
				float3 WorldSpacePosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldSpaceViewDirection = SafeNormalize( _WorldSpaceCameraPos.xyz  - WorldSpacePosition );
    
				float2 uv0_FlagTexture = IN.ase_texcoord7.xy * _FlagTexture_ST.xy + _FlagTexture_ST.zw;
				float cos12 = cos( radians( _TextureRotation ) );
				float sin12 = sin( radians( _TextureRotation ) );
				float2 rotator12 = mul( uv0_FlagTexture - float2( 0.5,0.5 ) , float2x2( cos12 , -sin12 , sin12 , cos12 )) + float2( 0.5,0.5 );
				
				float2 temp_cast_1 = (_FlagNormalScale).xx;
				float2 uv08 = IN.ase_texcoord7.xy * temp_cast_1 + float2( 0,0 );
				
				
		        float3 Albedo = ( _TextureColor * tex2D( _FlagTexture, rotator12 ) ).rgb;
				float3 Normal = tex2D( _FlagNormal, uv08 ).rgb;
				float3 Emission = 0;
				float3 Specular = float3(0.5, 0.5, 0.5);
				float Metallic = 0.24;
				float Smoothness = 0.24;
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = 0;

        		InputData inputData;
        		inputData.positionWS = WorldSpacePosition;

        #ifdef _NORMALMAP
        	    inputData.normalWS = normalize(TransformTangentToWorld(Normal, half3x3(WorldSpaceTangent, WorldSpaceBiTangent, WorldSpaceNormal)));
        #else
            #if !SHADER_HINT_NICE_QUALITY
                inputData.normalWS = WorldSpaceNormal;
            #else
        	    inputData.normalWS = normalize(WorldSpaceNormal);
            #endif
        #endif

        #if !SHADER_HINT_NICE_QUALITY
        	    // viewDirection should be normalized here, but we avoid doing it as it's close enough and we save some ALU.
        	    inputData.viewDirectionWS = WorldSpaceViewDirection;
        #else
        	    inputData.viewDirectionWS = normalize(WorldSpaceViewDirection);
        #endif

        	    inputData.shadowCoord = IN.shadowCoord;

        	    inputData.fogCoord = IN.fogFactorAndVertexLight.x;
        	    inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
        	    inputData.bakedGI = SAMPLE_GI(IN.lightmapUVOrVertexSH.xy, IN.lightmapUVOrVertexSH.xyz, inputData.normalWS);

        		half4 color = LightweightFragmentPBR(
        			inputData, 
        			Albedo, 
        			Metallic, 
        			Specular, 
        			Smoothness, 
        			Occlusion, 
        			Emission, 
        			Alpha);

			#ifdef TERRAIN_SPLAT_ADDPASS
				color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
			#else
				color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
			#endif

        #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif

		#if ASE_LW_FINAL_COLOR_ALPHA_MULTIPLY
				color.rgb *= color.a;
		#endif
        		return color;
            }

        	ENDHLSL
        }

		
        Pass
        {
			
        	Name "ShadowCaster"
            Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual

            HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x
            

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex ShadowPassVertex
            #pragma fragment ShadowPassFragment

            #define ASE_SRP_VERSION 51300


            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

            struct GraphVertexInput
            {
                float4 vertex : POSITION;
                float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

			float _Step;
			float _XFrequency;
			float _Speed;
			float _ZFrequency;
			float _ClampFactor;
			float _VerticleNoise;

        	struct VertexOutput
        	{
        	    float4 clipPos      : SV_POSITION;
                
                UNITY_VERTEX_INPUT_INSTANCE_ID
        	};

			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

            // x: global clip space bias, y: normal world space bias
            float3 _LightDirection;

            VertexOutput ShadowPassVertex(GraphVertexInput v )
        	{
        	    VertexOutput o;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);

				float myWorldPosX55 = v.vertex.xyz.x;
				float temp_output_34_0 = step( myWorldPosX55 , _Step );
				float mySpeed28 = _Speed;
				float temp_output_26_0 = ( _Time.y * mySpeed28 );
				float myWorldPosZ56 = v.vertex.xyz.z;
				float2 temp_cast_0 = (_VerticleNoise).xx;
				float temp_output_102_0 = ( _Time.y * 0.6 );
				float2 appendResult104 = (float2(temp_output_102_0 , temp_output_102_0));
				float2 uv0106 = v.ase_texcoord * temp_cast_0 + appendResult104;
				float simplePerlin2D107 = snoise( uv0106 );
				float smoothstepResult117 = smoothstep( -1.0 , 1.5 , simplePerlin2D107);
				float3 appendResult60 = (float3(( myWorldPosX55 / 100.0 ) , (-_ClampFactor + (( ( temp_output_34_0 * sin( ( ( _XFrequency * temp_output_26_0 ) + myWorldPosX55 ) ) ) * sin( ( myWorldPosZ56 + ( temp_output_26_0 * _ZFrequency ) ) ) ) - -1.0) * (_ClampFactor - -_ClampFactor) / (1.0 - -1.0)) , ( ( myWorldPosZ56 / 100.0 ) + ( temp_output_34_0 * smoothstepResult117 ) )));
				
				float3 vertexValue = appendResult60;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal =  v.ase_normal ;

        	    float3 positionWS = TransformObjectToWorld(v.vertex.xyz);
                float3 normalWS = TransformObjectToWorldDir(v.ase_normal);

                float invNdotL = 1.0 - saturate(dot(_LightDirection, normalWS));
                float scale = invNdotL * _ShadowBias.y;

                // normal bias is negative since we want to apply an inset normal offset
                positionWS = _LightDirection * _ShadowBias.xxx + positionWS;
				positionWS = normalWS * scale.xxx + positionWS;
                float4 clipPos = TransformWorldToHClip(positionWS);

                // _ShadowBias.x sign depens on if platform has reversed z buffer
                //clipPos.z += _ShadowBias.x;

        	#if UNITY_REVERSED_Z
        	    clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
        	#else
        	    clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
        	#endif
                o.clipPos = clipPos;

        	    return o;
        	}

            half4 ShadowPassFragment(VertexOutput IN  ) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);

               

				float Alpha = 1;
				float AlphaClipThreshold = AlphaClipThreshold;

         #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif
                return 0;
            }

            ENDHLSL
        }

		
        Pass
        {
			
        	Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }

            ZWrite On
			ColorMask 0

            HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex vert
            #pragma fragment frag

            #define ASE_SRP_VERSION 51300


            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			float _Step;
			float _XFrequency;
			float _Speed;
			float _ZFrequency;
			float _ClampFactor;
			float _VerticleNoise;

            struct GraphVertexInput
            {
                float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct VertexOutput
        	{
        	    float4 clipPos      : SV_POSITION;
                
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
        	};

			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
           

            VertexOutput vert(GraphVertexInput v  )
            {
                VertexOutput o = (VertexOutput)0;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float myWorldPosX55 = v.vertex.xyz.x;
				float temp_output_34_0 = step( myWorldPosX55 , _Step );
				float mySpeed28 = _Speed;
				float temp_output_26_0 = ( _Time.y * mySpeed28 );
				float myWorldPosZ56 = v.vertex.xyz.z;
				float2 temp_cast_0 = (_VerticleNoise).xx;
				float temp_output_102_0 = ( _Time.y * 0.6 );
				float2 appendResult104 = (float2(temp_output_102_0 , temp_output_102_0));
				float2 uv0106 = v.ase_texcoord * temp_cast_0 + appendResult104;
				float simplePerlin2D107 = snoise( uv0106 );
				float smoothstepResult117 = smoothstep( -1.0 , 1.5 , simplePerlin2D107);
				float3 appendResult60 = (float3(( myWorldPosX55 / 100.0 ) , (-_ClampFactor + (( ( temp_output_34_0 * sin( ( ( _XFrequency * temp_output_26_0 ) + myWorldPosX55 ) ) ) * sin( ( myWorldPosZ56 + ( temp_output_26_0 * _ZFrequency ) ) ) ) - -1.0) * (_ClampFactor - -_ClampFactor) / (1.0 - -1.0)) , ( ( myWorldPosZ56 / 100.0 ) + ( temp_output_34_0 * smoothstepResult117 ) )));
				
				float3 vertexValue = appendResult60;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal =  v.ase_normal ;

        	    o.clipPos = TransformObjectToHClip(v.vertex.xyz);
        	    return o;
            }

            half4 frag(VertexOutput IN  ) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);

				

				float Alpha = 1;
				float AlphaClipThreshold = AlphaClipThreshold;

         #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif
                return 0;
            }
            ENDHLSL
        }

        // This pass it not used during regular rendering, only for lightmap baking.
		
        Pass
        {
			
        	Name "Meta"
            Tags { "LightMode"="Meta" }

            Cull Off

            HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x

            #pragma vertex vert
            #pragma fragment frag

            #define ASE_SRP_VERSION 51300


			uniform float4 _MainTex_ST;
			
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/MetaInput.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			float _Step;
			float _XFrequency;
			float _Speed;
			float _ZFrequency;
			float _ClampFactor;
			float _VerticleNoise;
			float4 _TextureColor;
			sampler2D _FlagTexture;
			float4 _FlagTexture_ST;
			float _TextureRotation;

            #pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A
            #pragma shader_feature EDITOR_VISUALIZATION


            struct GraphVertexInput
            {
                float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct VertexOutput
        	{
        	    float4 clipPos      : SV_POSITION;
                float4 ase_texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
        	};

			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

            VertexOutput vert(GraphVertexInput v  )
            {
                VertexOutput o = (VertexOutput)0;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
				float myWorldPosX55 = v.vertex.xyz.x;
				float temp_output_34_0 = step( myWorldPosX55 , _Step );
				float mySpeed28 = _Speed;
				float temp_output_26_0 = ( _Time.y * mySpeed28 );
				float myWorldPosZ56 = v.vertex.xyz.z;
				float2 temp_cast_0 = (_VerticleNoise).xx;
				float temp_output_102_0 = ( _Time.y * 0.6 );
				float2 appendResult104 = (float2(temp_output_102_0 , temp_output_102_0));
				float2 uv0106 = v.ase_texcoord * temp_cast_0 + appendResult104;
				float simplePerlin2D107 = snoise( uv0106 );
				float smoothstepResult117 = smoothstep( -1.0 , 1.5 , simplePerlin2D107);
				float3 appendResult60 = (float3(( myWorldPosX55 / 100.0 ) , (-_ClampFactor + (( ( temp_output_34_0 * sin( ( ( _XFrequency * temp_output_26_0 ) + myWorldPosX55 ) ) ) * sin( ( myWorldPosZ56 + ( temp_output_26_0 * _ZFrequency ) ) ) ) - -1.0) * (_ClampFactor - -_ClampFactor) / (1.0 - -1.0)) , ( ( myWorldPosZ56 / 100.0 ) + ( temp_output_34_0 * smoothstepResult117 ) )));
				
				o.ase_texcoord.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord.zw = 0;

				float3 vertexValue = appendResult60;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal =  v.ase_normal ;
				
                o.clipPos = MetaVertexPosition(v.vertex, v.texcoord1.xy, v.texcoord2.xy, unity_LightmapST);
        	    return o;
            }

            half4 frag(VertexOutput IN  ) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);

           		float2 uv0_FlagTexture = IN.ase_texcoord.xy * _FlagTexture_ST.xy + _FlagTexture_ST.zw;
           		float cos12 = cos( radians( _TextureRotation ) );
           		float sin12 = sin( radians( _TextureRotation ) );
           		float2 rotator12 = mul( uv0_FlagTexture - float2( 0.5,0.5 ) , float2x2( cos12 , -sin12 , sin12 , cos12 )) + float2( 0.5,0.5 );
           		
				
		        float3 Albedo = ( _TextureColor * tex2D( _FlagTexture, rotator12 ) ).rgb;
				float3 Emission = 0;
				float Alpha = 1;
				float AlphaClipThreshold = 0;

         #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif

                MetaInput metaInput = (MetaInput)0;
                metaInput.Albedo = Albedo;
                metaInput.Emission = Emission;
                
                return MetaFragment(metaInput);
            }
            ENDHLSL
        }
		
    }
    FallBack "Hidden/InternalErrorShader"
	CustomEditor "ASEMaterialInspector"
	
	
}
/*ASEBEGIN
Version=16700
1010;121;1906;943;1134.745;-1097.835;1.677921;True;False
Node;AmplifyShaderEditor.CommentaryNode;80;-1703.413,299.4083;Float;False;562.8815;408.0076;Variables;6;67;24;28;57;56;55;Local Variables;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;24;-1598.946,348.5497;Float;False;Property;_Speed;Speed;7;0;Create;True;0;0;False;0;1;-4.6;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;126;-938.1243,559.2307;Float;False;1382.6;835.7552;Flag Ripple;17;59;58;35;34;37;31;30;27;23;42;41;40;39;38;26;25;29;Flag Ripple;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;28;-1373.234,349.6957;Float;False;mySpeed;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;125;-944.9291,1570.49;Float;False;1456.001;463.9005;Comment;11;120;106;104;114;102;103;101;117;119;107;118;Verticle Vertex Jiggle;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;29;-887.3647,1085.15;Float;False;28;mySpeed;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;25;-876.4093,1016.068;Float;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;67;-1632.051,481.8247;Float;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleTimeNode;101;-888.3821,1735.95;Float;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;103;-882.777,1812.009;Float;False;Constant;_Float0;Float 0;11;0;Create;True;0;0;False;0;0.6;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;55;-1370.538,439.405;Float;False;myWorldPosX;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;23;-666.7071,951.5316;Float;False;Property;_XFrequency;XFrequency;6;0;Create;True;0;0;False;0;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;26;-647.3647,1069.15;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;102;-688.5769,1766.11;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;104;-511.7776,1766.11;Float;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;59;-711.7532,747.138;Float;False;55;myWorldPosX;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;114;-583.566,1644.069;Float;False;Property;_VerticleNoise;Verticle Noise;11;0;Create;True;0;0;False;0;1;0.9;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;38;-654.2702,1278.654;Float;False;Property;_ZFrequency;ZFrequency;8;0;Create;True;0;0;False;0;1;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;27;-429.829,958.0433;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;56;-1370.622,599.4315;Float;False;myWorldPosZ;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;39;-416.5407,1259.276;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;30;-210.4776,957.7479;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;18;-937.4291,-805.4265;Float;False;1696.468;645.063;Texture;10;4;17;16;10;12;13;7;14;15;6;Flag Texture;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;58;-430.2517,1140.777;Float;False;56;myWorldPosZ;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;35;-402.3022,837.0515;Float;False;Property;_Step;Step;9;0;Create;True;0;0;False;0;0.03;4.39;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;106;-355.0768,1640.71;Float;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.NoiseGeneratorNode;107;-108.6223,1636.053;Float;True;Simplex2D;1;0;FLOAT2;0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;127;607.4952,815.649;Float;False;773.2719;388.545;Clamp Limit;5;52;82;54;83;43;Ripple Clamp Limit;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;40;-203.396,1147.336;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;34;-223.4881,781.7473;Float;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;31;19.95215,958.8669;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;6;-860.8635,-742.7202;Float;True;Property;_FlagTexture;Flag Texture;0;0;Create;True;0;0;False;0;b6a205d474915ee428aeb9b9a119dd09;b6a205d474915ee428aeb9b9a119dd09;False;white;Auto;Texture2D;0;1;SAMPLER2D;0
Node;AmplifyShaderEditor.RangedFloatNode;15;-563.4695,-262.3756;Float;False;Property;_TextureRotation;Texture Rotation;3;0;Create;True;0;0;False;0;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;118;-77.56003,1858.383;Float;False;Constant;_Float2;Float 2;11;0;Create;True;0;0;False;0;-1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;119;-73.26034,1932.183;Float;False;Constant;_Float5;Float 5;11;0;Create;True;0;0;False;0;1.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;121;910.327,1368.735;Float;False;56;myWorldPosZ;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;124;961.5819,1444.836;Float;False;Constant;_Float6;Float 6;11;0;Create;True;0;0;False;0;100;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SmoothstepOpNode;117;159.4401,1738.184;Float;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;13;-487.3556,-394.6005;Float;False;Constant;_Vector0;Vector 0;4;0;Create;True;0;0;False;0;0.5,0.5;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;54;664.1165,991.4588;Float;False;Constant;_Float3;Float 3;10;0;Create;True;0;0;False;0;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;7;-534.423,-513.5577;Float;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;37;155.445,870.815;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RadiansOpNode;14;-365.8693,-257.1754;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;41;-31.20854,1147.308;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;43;631.8052,1126.788;Float;False;Property;_ClampFactor;Clamp Factor;10;0;Create;True;0;0;False;0;0.3;0.4;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;120;374.138,1714.103;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;78;938.9443,647.3036;Float;False;Constant;_RealignScale;RealignScale;11;0;Create;True;0;0;False;0;100;5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;82;818.5082,1041.747;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;83;817.3525,937.757;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;123;1174.802,1373.273;Float;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;42;308.1604,871.0104;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;61;943.4804,562.891;Float;False;55;myWorldPosX;1;0;OBJECT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotatorNode;12;-186.4692,-433.9757;Float;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;77;1184.944,631.2472;Float;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;16;377.0347,-641.0356;Float;False;Property;_TextureColor;Texture Color;2;0;Create;True;0;0;False;0;1,1,1,0;1,1,1,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;129;1357.558,1503.788;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;10;291.9537,-466.2701;Float;True;Property;_TextureSample1;Texture Sample 1;3;0;Create;True;0;0;False;0;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;19;-938.2108,-10.79056;Float;False;1002.481;415.7195;Normal;7;2;3;1;9;11;8;5;Normal Texture;1,1,1,1;0;0
Node;AmplifyShaderEditor.TFHCRemapNode;52;1202.038,866.3968;Float;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;17;616.1975,-520.7786;Float;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;8;-594.3576,250.7116;Float;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;11;-270.0917,43.36796;Float;True;Property;_TextureSample0;Texture Sample 0;1;0;Create;True;0;0;False;0;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;4;-827.7528,-496.4385;Float;False;Property;_FlagTextureScale;Flag Texture Scale;1;0;Create;True;0;0;False;0;1;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;57;-1368.61,522.3537;Float;False;myWorldPosY;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;9;-591.2397,44.92695;Float;True;Property;_FlagNormal;Flag Normal;4;0;Create;True;0;0;False;0;None;a88be22e1601b234da15641a48aab200;True;white;Auto;Texture2D;0;1;SAMPLER2D;0
Node;AmplifyShaderEditor.DynamicAppendNode;60;1822.473,998.6722;Float;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;128;1722.275,340.6057;Float;False;Constant;_Float7;Float 7;11;0;Create;True;0;0;False;0;0.24;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;5;-889.0037,253.8293;Float;False;Property;_FlagNormalScale;Flag Normal Scale;5;0;Create;True;0;0;False;0;1;3.75;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;0,0;Float;False;False;2;Float;ASEMaterialInspector;0;2;Hidden/Templates/LightWeightSRPPBR;1976390536c6c564abb90fe41f6ee334;True;ShadowCaster;0;1;ShadowCaster;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;;0;0;Standard;0;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;2;Float;ASEMaterialInspector;0;2;Hidden/Templates/LightWeightSRPPBR;1976390536c6c564abb90fe41f6ee334;True;Meta;0;3;Meta;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;True;2;False;-1;False;False;False;False;False;True;1;LightMode=Meta;False;0;;0;0;Standard;0;6;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;2;Float;ASEMaterialInspector;0;2;Hidden/Templates/LightWeightSRPPBR;1976390536c6c564abb90fe41f6ee334;True;DepthOnly;0;2;DepthOnly;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;False;True;False;False;False;False;0;False;-1;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;;0;0;Standard;0;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;2020.932,225.2219;Float;False;True;2;Float;ASEMaterialInspector;0;2;Rod/ASE-LWRP-Flag;1976390536c6c564abb90fe41f6ee334;True;Base;0;0;Base;11;False;False;False;True;2;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;6;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;True;True;True;True;True;0;False;-1;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=LightweightForward;False;0;;0;0;Standard;2;Vertex Position,InvertActionOnDeselection;1;Receive Shadows;1;1;_FinalColorxAlpha;0;4;True;True;True;True;False;11;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;9;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT3;0,0,0;False;10;FLOAT3;0,0,0;False;0
WireConnection;28;0;24;0
WireConnection;55;0;67;1
WireConnection;26;0;25;0
WireConnection;26;1;29;0
WireConnection;102;0;101;0
WireConnection;102;1;103;0
WireConnection;104;0;102;0
WireConnection;104;1;102;0
WireConnection;27;0;23;0
WireConnection;27;1;26;0
WireConnection;56;0;67;3
WireConnection;39;0;26;0
WireConnection;39;1;38;0
WireConnection;30;0;27;0
WireConnection;30;1;59;0
WireConnection;106;0;114;0
WireConnection;106;1;104;0
WireConnection;107;0;106;0
WireConnection;40;0;58;0
WireConnection;40;1;39;0
WireConnection;34;0;59;0
WireConnection;34;1;35;0
WireConnection;31;0;30;0
WireConnection;117;0;107;0
WireConnection;117;1;118;0
WireConnection;117;2;119;0
WireConnection;7;2;6;0
WireConnection;37;0;34;0
WireConnection;37;1;31;0
WireConnection;14;0;15;0
WireConnection;41;0;40;0
WireConnection;120;0;34;0
WireConnection;120;1;117;0
WireConnection;82;0;43;0
WireConnection;83;0;54;0
WireConnection;123;0;121;0
WireConnection;123;1;124;0
WireConnection;42;0;37;0
WireConnection;42;1;41;0
WireConnection;12;0;7;0
WireConnection;12;1;13;0
WireConnection;12;2;14;0
WireConnection;77;0;61;0
WireConnection;77;1;78;0
WireConnection;129;0;123;0
WireConnection;129;1;120;0
WireConnection;10;0;6;0
WireConnection;10;1;12;0
WireConnection;52;0;42;0
WireConnection;52;1;83;0
WireConnection;52;2;54;0
WireConnection;52;3;82;0
WireConnection;52;4;43;0
WireConnection;17;0;16;0
WireConnection;17;1;10;0
WireConnection;8;0;5;0
WireConnection;11;0;9;0
WireConnection;11;1;8;0
WireConnection;57;0;67;2
WireConnection;60;0;77;0
WireConnection;60;1;52;0
WireConnection;60;2;129;0
WireConnection;0;0;17;0
WireConnection;0;1;11;0
WireConnection;0;3;128;0
WireConnection;0;4;128;0
WireConnection;0;8;60;0
ASEEND*/
//CHKSM=77C6E957EE8A6660818B1634433B14C8746DC51A