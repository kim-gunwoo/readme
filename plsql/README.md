# readme
```sql
CREATE OR REPLACE FUNCTION US_HRMAPP.SF_SV_HOLIXM
(
     ar_EMPL_NUMB HR_MASTXM.EMPL_NUMB%TYPE  --사원번호
    ,ar_STDS_DATE VARCHAR2 DEFAULT TO_CHAR(SYSDATE,'YYYYMMDD') --기준일
    ,ar_USED_TYPE VARCHAR2 DEFAULT '1' --사용일수 '1' : 전체, '0' : 기준일까지
    ,ar_GUBUN VARCHAR2 DEFAULT '01' --일반적인 사용  01 :  급여사용 02
)
RETURN TTYPE_SV_HOLIXM1 AS

    v_tab TTYPE_SV_HOLIXM1 := TTYPE_SV_HOLIXM1();

    v_PCOP_JCDT HR_MASTXM.PCOP_JCDT%TYPE;  --공단입사일
    v_ANSD_DATE HR_MASTXM.ANSD_DATE%TYPE;  --연차기준일

    v_ANSD_DATE_W HR_MASTXM.ANSD_DATE%TYPE;  --연차기준일(원본)

    v_RERN_NUMB HR_MASTXM.RERN_NUMB%TYPE;  --주민등록번호
    v_CBGB_CODE HR_MASTXM.CBGB_CODE%TYPE;  --보험/의료사업구분

    v_ORGC_DCNT NUMBER(6,3);  --원 발생일수
    v_OCUR_DCNT NUMBER(6,3);  --연차발생일수
    v_AOCU_DCNT NUMBER(6,3);  --추가발생일수
    v_PUSE_DCNT NUMBER(6,3);  --선사용차감일수
    v_USEX_DCNT NUMBER(6,3);  --사용일수

    v_DGNL_DCNT NUMBER(6,3);  --무단결근, 무급휴과일수

    v_POCR_DCNT NUMBER(6,3);  --선사용발생일수
    v_PUSX_DCNT NUMBER(6,3);  --선사용사용일수

    v_ACUR_DCNT NUMBER(6,3);  --대체휴가(가정의날) 발생일수
    v_AUSX_DCNT NUMBER(6,3);  --대체휴가(가정의날) 사용일수

    v_CONT_YEAR NUMBER(3,0);  --근무년차

    v_LAST_ANDT VARCHAR2(8);  --최근연차기준일
    v_NEXT_ANDT VARCHAR2(8);  --다음연차기준일
    v_NEXT_OCUR_DCNT NUMBER(6,3); --다음연차발생일수
    v_NEXT_CONT_YEAR NUMBER(3,0);  --다음연차기준일 기준 근무년차

    v_BJIK_LAST_ANDT VARCHAR2(8); --복직기준최근연차기준일

    v_BJIK_YSNO INT;  --전년도 복직여부
    v_BJIK_YSNO2 INT; --당해복직여부

    v_POCR_DCNT_TEMP NUMBER(5,0);  -- 선사용 일수 확인을 위해 잠시 만들어 둔다
    v_NEXT_OCUR_DCNT_TEMP NUMBER(6,0);  -- 다음연차발생일수 /2,나머지 버림.
    v_NEXT_OCUR_DCNT_TEMP1 NUMBER(6,0);  -- 다음정상연차 발생일수 (휴직자들의 경우 변경이 되므로 변경 전 값을 미리 넣어준다.)
    v_EXCP_ENDD VARCHAR2(8);  --휴직 마지막 일자
    v_WORK_RATE_00_TEMP NUMBER;  --육아 휴직 출근율
    v_NEXT_OCUR_DCNT1 NUMBER(6,3);  --휴직자들의 내년도 연차를 계산하기 위해 만듬.
    v_LAST_ANDT1 VARCHAR2(8);  --전전년도 연차기준일
    v_NEXT_ANDT1 VARCHAR2(8);  --전년도 연차기준일

    v_WORK_DCNT INT;        --평일일수
    v_EXDV_DCNT_00 INT;  --육아휴직일수
    v_EXDV_DCNT INT;     --육아휴직외 휴직일수
    v_WORK_RATE_00 NUMBER;  --육아휴직 포함 출근율
    v_WORK_RATE NUMBER;     --육아휴직제외 출근율

    --휴직전 연차 남은 갯수 , 복직 후 미사용 하여 이월한 갯수
    v_IWOL_CNT1 NUMBER(6,3);
    v_IWOL_CNT2 NUMBER(6,3);

    --지참, 외출, 조퇴 시간(질병외)
    v_USEA_DCNT NUMBER(6,3);

    --휴직정보 존재여부
    v_EXJA_CONT INT;
    v_EXJA_CONT_1 INT;  --SJY

    --조정일수
    v_ADJT_DCNT NUMBER(6,3);

    --MHS 2015.3.19 사용(조정) 추가  인사부 박성민 과장 요청 ( 잔여일수에서 추가로 빼준다)
    v_AUSE_DCNT NUMBER(6,3);

    -- 2013.07.추가
    v_EXCP_DATE VARCHAR2(8);
    v_EXCP_BEDE1 VARCHAR2(8);  --최근연차기준일과 기준년도 휴직시작일
    v_EXCP_ENDD1 VARCHAR2(8);  --최근연차기준일과 기준년도 휴직시작일 사이의 휴직종료일
    v_BEFORE_DCNT_TEMP NUMBER(6,0);

    v_GUBUN NUMBER;     --

    --MHS 2015.3.12 의료연차조정기준일(급여) 노사협력부 김정화 대리 요청
    v_ANJJ_DATE  PY_MASTPM.ANJJ_DATE%TYPE;  --연차기준일
    v_CBGB_CODE2  PY_MASTPM.CBGB_CODE2%TYPE;  --의료구분 2

    -- 2018.08.02 육아휴직 1년차
    v_PJH_CNT0  NUMBER; --소정근로일수
    v_PJH_CNT1  NUMBER; --법정휴직
    v_PJH_CNT2  NUMBER; --비법정휴직

    v_NOPY_DCNT  NUMBER; --무급휴가
    /******************************************************************************
     NAME    :  SF_SV_HOLIXM
     PURPOSE :  연차구하기 위한 함수

     REVISIONS:
     Ver            Date            Author                           Description
     ---  --------------   ---------------  ------------------------------------
     1.0      2012-10-12            김춘호              1. Created this function.
     NOTES:
     * 연차관련 정보
    ******************************************************************************/

    PRAGMA autonomous_transaction;

BEGIN

    v_OCUR_DCNT := 0;  --연차발생일수
    v_AOCU_DCNT := 0;  --추가발생일수
    v_PUSE_DCNT := 0;  --선사용차감일수
    v_USEX_DCNT := 0;  --사용일수
    v_POCR_DCNT := 0;  --선사용발생일수
    v_PUSX_DCNT := 0;  --선사용사용일수

    v_DGNL_DCNT := 0;  -- 결근, 무급휴가 일수

    v_GUBUN := 0;  --구분

    BEGIN
        --입사일, 연차기준일 구하기
        SELECT  A.PCOP_JCDT
               ,A.ANSD_DATE
               ,A.CBGB_CODE
               ,A.RERN_NUMB
               ,B.ANJJ_DATE
               ,B.CBGB_CODE2
          INTO  v_PCOP_JCDT   --공단입사일
               ,v_ANSD_DATE   --연차기준일
               ,v_CBGB_CODE   --보험/의료구분
               ,v_RERN_NUMB   --주민등록번호
               ,v_ANJJ_DATE   --의료연차조정기준일(급여
               ,v_CBGB_CODE2  --의료구분
          FROM HR_MASTXM A LEFT OUTER JOIN PY_MASTPM B ON A.EMPL_NUMB = B.EMPL_NUMB
         WHERE A.EMPL_NUMB = ar_EMPL_NUMB;

        --MHS 2015.3.23 연차기준일을 원본일자을 저장
        v_ANSD_DATE_W := v_ANSD_DATE;

        --MHS 2015.3.12 의료연차조정기준일(급여) 노사협력부 김정화 대리 요청
        IF v_CBGB_CODE2 = '2' AND ar_GUBUN = '02' THEN --급여에서 호출했으면  의료연차조정기준일(급여)을 연차일로 한다

            v_ANSD_DATE := NVL(v_ANJJ_DATE,v_ANSD_DATE);
        END IF;
        --END MHS 2015.3.12

        --입사일이 기준일 보다 적은 경우 리턴
        IF v_PCOP_JCDT IS NULL OR v_ANSD_DATE IS NULL OR ar_STDS_DATE < v_PCOP_JCDT THEN
            GOTO RTNVALUE;
        END IF;

		/************************* 최근연차 기준일 *****************************************/
        --최근연차기준일
        v_LAST_ANDT := CASE
							WHEN SUBSTR(ar_STDS_DATE,5,4) < SUBSTR(v_ANSD_DATE, 5,4) THEN
								TO_CHAR(TO_NUMBER(SUBSTR(ar_STDS_DATE,1,4)) - 1)
							ELSE SUBSTR(ar_STDS_DATE,1,4)
					  END || SUBSTR(v_ANSD_DATE,5,4);
        v_LAST_ANDT1 := CASE
							WHEN SUBSTR(ar_STDS_DATE,5,4) <= SUBSTR(v_ANSD_DATE, 5,4)
								THEN TO_CHAR(TO_NUMBER(SUBSTR(ar_STDS_DATE,1,4)) - 1)
							ELSE SUBSTR(ar_STDS_DATE,1,4)
						END || SUBSTR(v_ANSD_DATE,5,4);

		-- 최근연차기준일 윤년에 따라서 재 계산
        IF SUBSTR(v_LAST_ANDT,5,4) = '0229' THEN
            IF MOD(TO_NUMBER(SUBSTR(v_LAST_ANDT,1,4)),4) <> 0 THEN
                v_LAST_ANDT := SUBSTR(v_LAST_ANDT,1,4) || '0301';
            END IF;
        END IF;
        /************************* 최근연차 기준일 *****************************************/

		/************************* 다음연차 기준일 *****************************************/
		--다음연차기준일 (최근연차기준일 12개월뒤)
        v_NEXT_ANDT := TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT,'YYYYMMDD'), 12),'YYYYMMDD');
--        v_NEXT_ANDT1 := TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT1,'YYYYMMDD'), 12),'YYYYMMDD');
        /************************* 다음연차 기준일 *****************************************/

		/************************* 근무년차 구하기(연차기준일 기준) *****************************************/
        --근무년차 구하기(연차기준일 기준)
        v_CONT_YEAR := CEIL(MONTHS_BETWEEN(TO_DATE(ar_STDS_DATE,'YYYYMMDD') + 1,TO_DATE(v_ANSD_DATE,'YYYYMMDD'))/12) + 1;
		/************************* 근무년차 구하기(연차기준일 기준) *****************************************/

        -- 근무년차에 따라 연차발생일수 산정

		/************************* 근무년차에 따라 연차발생일수 산정 *****************************************/
        IF v_CONT_YEAR < 1 THEN
            v_OCUR_DCNT := 0;
        ELSIF v_CONT_YEAR = 1 THEN
            --1년차의 경우 매월 입사일자에 1개씩 발생
            v_OCUR_DCNT := TRUNC(MONTHS_BETWEEN(TO_DATE(ar_STDS_DATE,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD')),0);

            SELECT SF_GET_NOPY(v_RERN_NUMB,v_PCOP_JCDT,ar_STDS_DATE,v_ANSD_DATE_W)
              INTO v_NOPY_DCNT
              FROM DUAL;

              v_OCUR_DCNT := v_OCUR_DCNT - v_NOPY_DCNT;  -- 무급,결근 일수 차감
        ELSIF v_CONT_YEAR = 2 THEN
            --근속년수 구하기
            IF TRUNC(MONTHS_BETWEEN(TO_DATE(ar_STDS_DATE,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD'))/12,0) < 1 THEN
                --2년차의 경우 만1년이 되기까지는 매월 1개씩 발생
                v_OCUR_DCNT := TRUNC(MONTHS_BETWEEN(TO_DATE(ar_STDS_DATE,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD')),0);

                --2년차의 경우 만1년이 되기까지는 아래 로직 적용 (7. 21. LHG)
                SELECT SF_GET_NOPY(v_RERN_NUMB,v_PCOP_JCDT,ar_STDS_DATE,v_ANSD_DATE_W)
                  INTO v_NOPY_DCNT
                  FROM DUAL;

                 v_OCUR_DCNT := v_OCUR_DCNT - v_NOPY_DCNT;  -- 무급,결근 일수 차감

            ELSE
                --만1년이 도래한 경우 15개 발생, 2017.05.30 이후 입사자는 26개 발생
                IF v_PCOP_JCDT < '20170530' THEN --SJY 2018.05.28
                   v_OCUR_DCNT := 15;
                ELSE
                    v_OCUR_DCNT := 26; --SJY 2018.05.28, 매월 한개씩 발생하여 11개 + 15
                END IF;

            END IF;
        ELSIF v_CONT_YEAR = 3 THEN
            --3년차의 경우 만1년시점에 발생한 연차갯수 + 월수에 비례하여 추가
--            v_OCUR_DCNT := 15;
                IF v_PCOP_JCDT < '20170530' THEN --SJY 2018.05.28
                   v_OCUR_DCNT := 15;
                ELSE
                    v_OCUR_DCNT := 26; --SJY 2018.05.28, 매월 한개씩 발생하여 11개 + 15
                END IF;

        ELSIF v_CONT_YEAR >= 4 THEN
            v_OCUR_DCNT := 15 + TRUNC((v_CONT_YEAR-3)/2);
            IF v_OCUR_DCNT > 25 THEN
                v_OCUR_DCNT := 25;
            END IF;


        END IF;

        v_ORGC_DCNT := v_OCUR_DCNT; -- 원발생일수
        /************************* 근무년차에 따라 연차발생일수 산정 *****************************************/
        --휴직기간에 따라 차감..

        --출근율 = 근무일수/평일,
        --1년차 육아휴직 출근율 = 근무일수-휴직일수/평일-휴직일수. 단, 연차발생일수 - (연차발생일수 * 휴직일수/평일).
        --출근율 80%이상 인경우 정상부여, 미만인경우 0.

		/************************* 근속제외자 등록 여부 기관 상관 없음 *****************************************/
        SELECT COUNT(EMPL_NUMB)
		  INTO v_EXJA_CONT
          FROM HR_EXJAIM
         WHERE EMPL_NUMB IN (
								SELECT EMPL_NUMB
								  FROM HR_MASTXM
								 WHERE RERN_NUMB =  v_RERN_NUMB
								   AND PCOP_JCDT =  v_PCOP_JCDT
								   AND ANSD_DATE = v_ANSD_DATE_W
							);
      /************************* 자녀양육 최초 1년 근속제외자 등록 여부(최근연차기준일포함이전날짜로) *****************************************/
        --SJY 2018.06.28 ST
        SELECT COUNT(EMPL_NUMB)
		  INTO v_EXJA_CONT_1
          FROM HR_EXJAIM
         WHERE EMPL_NUMB IN (
								SELECT EMPL_NUMB
								  FROM HR_MASTXM
								 WHERE RERN_NUMB =  v_RERN_NUMB
								   AND PCOP_JCDT =  v_PCOP_JCDT
								   AND ANSD_DATE = v_ANSD_DATE_W
							 )
           AND EXCP_BEDE >= '20171221'		-- 제외시작일 >= '20171221'
           AND EXDV_CODE = '00'				-- 근속제외구분코드 : 자녀양육 최조 1년
           AND EXCP_BEDE <= v_LAST_ANDT;	-- 제외시작일 <= 최근연차기준일
        --SJY 2018.06.28 END
	   /************************* 근속제외자 등록 여부 *****************************************/
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        /*********** 연차발생일수 **********/
		IF v_EXJA_CONT > 0 THEN -- 근속제외자 등록 된 경우 기관 상관 없음
            IF v_EXJA_CONT_1 > 0 THEN --2017.12.21. 이후 자녀양육 최초 1년 근속제외자 등록 여부(최근연차기준일포함이전날짜로)
                --소정근로일수
				-- 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 평일일수
                SELECT COUNT(A.CALN_DATE)
                INTO v_PJH_CNT0
                FROM TM_CALDXM A
                WHERE A.HOGB_CODE = '01'	-- 평일
                AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
						AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD'); -- 최근연차기준일 직전기준일 기준 연차기준일시작일~연차기준일종료일

                --법정휴직(육아휴직(1년)) 군휴직은 근속제외테이블에 정보없음
				-- 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 법정휴직(육아휴직(1년)) 사용한 평일일수
                SELECT COUNT(A.CALN_DATE)
                INTO v_PJH_CNT1
                FROM TM_CALDXM A, HR_EXJAIM B
                WHERE B.EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB = v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
                AND B.EXDV_CODE = '00'  --육아휴직 1년차
                AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
					AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                AND A.CALN_DATE BETWEEN B.EXCP_BEDE AND B.EXCP_ENDD
                AND A.HOGB_CODE = '01'  --평일
                ;


                --비법정휴직(육아휴직(1년)) 군휴직은 근속제외테이블에 정보없음
				-- 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 법정휴직(육아휴직(1년))외 사용한 평일일수
                SELECT COUNT(A.CALN_DATE)
                INTO v_PJH_CNT2
                FROM TM_CALDXM A, HR_EXJAIM B
                WHERE B.EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
                AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
					AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                AND A.CALN_DATE BETWEEN B.EXCP_BEDE AND B.EXCP_ENDD
                AND B.EXDV_CODE <> '00' --육아휴직 1년차
                AND A.HOGB_CODE = '01' --평일
                ;

                -- 비법정휴직일을 제외한 근무율
                v_WORK_RATE := (v_PJH_CNT0 - v_PJH_CNT2) / (v_PJH_CNT0);
				--				(최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 평일일수 - 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 법정휴직(육아휴직(1년))외 사용한 평일일수 ) / 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 법정휴직(육아휴직(1년))외 사용한 평일일수

				/*************** 근뮤율에 따른 연차발생일 계산 **************************/
                IF v_WORK_RATE >= 0.8 THEN -- 비법정휴직일을 제외한 근무율 80% 이상인 경우
                    IF ((v_PJH_CNT0 - v_PJH_CNT2) / v_PJH_CNT0) >= 0.8 THEN -- 비법정휴직일을 제외한 근무율 80% 이상인 경우
                        v_OCUR_DCNT := v_ORGC_DCNT;
                        -- 연차발생일수 := 원발생일수

                    ELSE													-- 비법정휴직일을 제외한 근무율 80% 미만인 경우
                        v_OCUR_DCNT := CEIL(v_ORGC_DCNT * ((v_PJH_CNT0 - v_PJH_CNT2) / v_PJH_CNT0));
						-- 연차발생일수 := 원발생일수* ((최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 평일일수 - 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 법정휴직(육아휴직(1년))외 사용한 평일일수) / 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 평일일수))
                    END IF;
                ELSE					-- 비법정휴직일을 제외한 근무율 80% 미만인 경우
					-- 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 법정휴직(육아휴직(1년))외 사용한 평일 일자 외 평일의 달별 평일일수와
					-- 근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 달별 평일일수가 같은 경우 1일 발생
                    SELECT NVL(SUM(
									CASE
										WHEN COUNT(*) = (
															SELECT COUNT(*)
															  FROM TM_CALDXM
															 WHERE SUBSTR(CALN_DATE, 1, 6) = (SUBSTR(A.CALN_DATE, 1, 6))
															   AND HOGB_CODE = '01'
														) THEN 1
										ELSE 0
								    END
								   ), 0)
                    INTO v_OCUR_DCNT
                    FROM TM_CALDXM A
                    WHERE A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
						AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                    AND A.HOGB_CODE = '01'  --평일
                    AND A.CALN_DATE NOT IN (/* 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 법정휴직(육아휴직(1년))외 사용한 평일 일자 */
											SELECT A.CALN_DATE
                                              FROM TM_CALDXM A
												INNER JOIN (
															SELECT EXCP_BEDE, EXCP_ENDD
															 FROM HR_EXJAIM
															 WHERE EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
															   AND EXDV_CODE <> '00'
															 ) B ON A.CALN_DATE BETWEEN B.EXCP_BEDE AND B.EXCP_ENDD
                                             WHERE A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
													AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                                               AND A.HOGB_CODE = '01')  --평일)
                    GROUP BY SUBSTR(CALN_DATE, 1, 6);

                END IF;
				/*************** 근뮤율에 따른 연차발생일 계산 **************************/
			-- 2017.12.21 이전
            ELSE 	--2017.12.21. 이후 자녀양육 최초 1년 근속제외자 등록 여부(최근연차기준일포함이전날짜로)가 아닌 경우
                --평일 일수
				-- 최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 평일일수
                SELECT COUNT(A.CALN_DATE) INTO v_WORK_DCNT
                  FROM TM_CALDXM A
                 WHERE A.HOGB_CODE = '01'
                   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                                           AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD');

                SELECT COUNT(CASE WHEN A.EXDV_CODE IN ('00','08') THEN A.EXDV_CODE END) --육아휴직일수, 가족돌봄휴직일수
                      ,COUNT(CASE WHEN A.EXDV_CODE <> '00' THEN A.EXDV_CODE END)  --일반휴직일수
                  INTO v_EXDV_DCNT_00
				    , v_EXDV_DCNT
                  FROM (--최근 연차기준일 직전기준일 기준 연차사용시작일~ 연차사용종료일의 연차산정시제외코드가 1인건의 평일일자와 코드
						SELECT A.CALN_DATE
							  , MAX(B.EXDV_CODE) AS EXDV_CODE
						  FROM TM_CALDXM A
						   INNER JOIN HR_EXJAIM B ON A.CALN_DATE BETWEEN B.EXCP_BEDE AND NVL(B.EXCP_ENDD,'29991231')
							 AND B.EXCP_BEDE <= TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')	-- 시작일 <= 최근연차 기준일 직전기준일 연차사용 종료일
							 AND NVL(B.EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD') -- 종료일 >= 최근연차 기준일 직전기준일 연차사용 시작일
							 AND B.EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
							 AND SF_GET_REFXFILD('EXDV_CODE', B.EXDV_CODE, '1') = '1' -- 보조필드 1이 1인 경우
						 WHERE A.HOGB_CODE = '01' -- 평일
						   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
											   AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
						 GROUP BY A.CALN_DATE
                 ) A;


				--select * from HR_EXJAIM where EMPL_NUMB = '30308'
                --출근율
				--육아 휴직 출근율
                v_WORK_RATE_00 := CASE
									WHEN v_WORK_DCNT - v_EXDV_DCNT_00 = 0 THEN 0
									ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / (v_WORK_DCNT - v_EXDV_DCNT_00)
								  END;
				-- 육아휴직제외 출근율
                v_WORK_RATE := CASE
								WHEN v_WORK_DCNT = 0 THEN 0
								ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / v_WORK_DCNT
							   END;

                --휴직종료일(2013.07.22)
				-- 연차산정제외 근속제외일의 마지막 휴직 종료일
                SELECT MAX(EXCP_ENDD)
                  INTO v_EXCP_DATE
                  FROM HR_EXJAIM
    --                         WHERE NVL(EXCP_ENDD,'29991231') <= v_LAST_ANDT
    --                           AND NVL(EXCP_ENDD,'29991231') > TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
    --                           AND
                   WHERE EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
                   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

				-- 복직기준최근연차기준일
				-- 복귀일 : 2022-08-01이고, 연차기준일 : 1995-01-01인 경우 2022-01-01
				v_BJIK_LAST_ANDT := CASE
										WHEN SUBSTR(v_EXCP_DATE,5,4) < SUBSTR(v_ANSD_DATE, 5,4) THEN
											TO_CHAR(TO_NUMBER(SUBSTR(v_EXCP_DATE,1,4)) - 1)
										ELSE SUBSTR(v_EXCP_DATE,1,4)
									END || SUBSTR(v_ANSD_DATE,5,4);

            ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

				--최근연차기준일 직전기준일 부터~ 다음연차기준일사이의 가장 늦은 휴직시작일( 연차산정시제외코드가 1인건)
				SELECT MAX(EXCP_BEDE)
				  INTO v_EXCP_BEDE1
				  FROM HR_EXJAIM
				 WHERE NVL(EXCP_BEDE,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
				   AND NVL(EXCP_BEDE,'29991231') < TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), 12), 'YYYYMMDD')
				   AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE  RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
				   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

				-- 최근연차기준일 직전기준일 부터 (최근연차기준일 직전기준일 부터~ 다음연차기준일사이의 가장 늦은 휴직시작일)의 가장 늦은 휴직종료일( 연차산정시제외코드가 1인건)
				SELECT MAX(EXCP_ENDD)
				  INTO v_EXCP_ENDD1
				  FROM HR_EXJAIM
				 WHERE NVL(EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
				   AND NVL(EXCP_ENDD,'29991231') < v_EXCP_BEDE1
				   AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
				   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1'
				   ;

				--????
               SELECT CASE WHEN v_EXCP_ENDD1 IS NULL THEN
								TRUNC(MONTHS_BETWEEN(
														TO_DATE(v_EXCP_BEDE1,'YYYYMMDD')
														,ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12)
													))  --근무가능월수 체크
						   WHEN v_EXCP_ENDD1 IS NOT NULL THEN
								TRUNC(MONTHS_BETWEEN(
														TO_DATE(v_EXCP_BEDE1,'YYYYMMDD')
														,TO_DATE(v_EXCP_ENDD1,'YYYYMMDD')
													))  --근무가능월수 체크
					   END
               INTO v_BEFORE_DCNT_TEMP
               FROM DUAL;

            ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

				/*
					v_WORK_RATE_00   --육아휴직 포함 출근율
					v_WORK_RATE      --육아휴직제외 출근율
				*/
              --연차발생일
                IF v_WORK_RATE >= 0.8 THEN
                    v_GUBUN := 1;  --구분
                    v_OCUR_DCNT := v_OCUR_DCNT;

                ELSIF v_WORK_RATE_00 >= 0.8 THEN
                    v_GUBUN := 2;  --구분

                    v_OCUR_DCNT :=  v_OCUR_DCNT - TRUNC(v_OCUR_DCNT * (v_EXDV_DCNT_00 + v_EXDV_DCNT) / v_WORK_DCNT,0);

    --            ELSIF v_WORK_RATE < 0.8 AND  v_WORK_RATE_00 < 0.8 AND (CEIL(MONTHS_BETWEEN(TO_DATE(v_LAST_ANDT,'YYYYMMDD') , TO_DATE(v_EXCP_DATE, 'YYYYMMDD'))) BETWEEN 0 AND 12) THEN
                ELSIF v_WORK_RATE < 0.8 AND  v_WORK_RATE_00 < 0.8 AND v_LAST_ANDT > v_BJIK_LAST_ANDT
						AND (
								(v_CBGB_CODE = '1'  AND v_BJIK_LAST_ANDT > '20121231')
								OR (v_CBGB_CODE = '2' AND  v_BJIK_LAST_ANDT > '20120801')
							)  THEN
                    v_GUBUN := 3;  --구분
    --               IF v_BJIK_LAST_ANDT
                --2013.07.22 선사용 발생을 연차 발생일로

					SELECT COUNT(EMPL_NUMB)
					  INTO v_BJIK_YSNO2
					  FROM HR_EXJAIM
					 WHERE NVL(EXCP_ENDD,'29991231') <= v_LAST_ANDT
					   AND NVL(EXCP_ENDD,'29991231') < TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), 12), 'YYYYMMDD')
					   AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
					   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

					--발령종료일
					SELECT MAX(EXCP_ENDD)
					  INTO v_EXCP_ENDD
					  FROM HR_EXJAIM
					 WHERE NVL(EXCP_ENDD,'29991231') <= v_LAST_ANDT
					   AND NVL(EXCP_ENDD,'29991231') < TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), 12), 'YYYYMMDD')
					   AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
					   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

					--다음연차기준일 기준 발생일수
					v_NEXT_CONT_YEAR := CEIL(MONTHS_BETWEEN(TO_DATE(v_LAST_ANDT,'YYYYMMDD') + 1,TO_DATE(v_ANSD_DATE,'YYYYMMDD'))/12) + 1;

					IF v_NEXT_CONT_YEAR < 1 THEN
						v_NEXT_OCUR_DCNT := 0;
					ELSIF v_NEXT_CONT_YEAR = 1 THEN
						--1년차의 경우 매월 입사일자에 1개씩 발생
						v_NEXT_OCUR_DCNT := TRUNC(MONTHS_BETWEEN(TO_DATE(v_LAST_ANDT,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD')),0);
					ELSIF v_NEXT_CONT_YEAR = 2 THEN
						--근속년수 구하기
						IF TRUNC(MONTHS_BETWEEN(TO_DATE(v_LAST_ANDT,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD'))/12,0) < 1 THEN
							--2년차의 경우 만1년이 되기까지는 매월 1개씩 발생
							v_NEXT_OCUR_DCNT := TRUNC(MONTHS_BETWEEN(TO_DATE(v_LAST_ANDT,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD')),0);
						ELSE
							--만1년이 도래한 경우 15개 발생
							IF v_PCOP_JCDT < '20170530' THEN --SJY 2018.05.28
							   v_NEXT_OCUR_DCNT := 15;
							ELSE
							   v_NEXT_OCUR_DCNT := 26; --이부분일거 같음..SJY, 매월 한개씩 발생하여 11개 + 15
							END IF;

						END IF;
					ELSIF v_NEXT_CONT_YEAR = 3 THEN
						--3년차의 경우 만1년시점에 발생한 연차갯수 + 월수에 비례하여 추가
						IF v_PCOP_JCDT < '20170530' THEN  --SJY 2018.05.28
							v_NEXT_OCUR_DCNT := 15;
						ELSE
							v_NEXT_OCUR_DCNT := 26; --이부분일거 같음..SJY 2018.05.28, 매월 한개씩 발생하여 11개 + 15
						END IF;

					ELSIF v_NEXT_CONT_YEAR >= 4 THEN
						v_NEXT_OCUR_DCNT := 15 + TRUNC((v_NEXT_CONT_YEAR-3)/2);
						IF v_NEXT_OCUR_DCNT > 25 THEN
							v_NEXT_OCUR_DCNT := 25;
						END IF;
					END IF;

					v_NEXT_OCUR_DCNT_TEMP1 := v_NEXT_OCUR_DCNT;


				   -- 전년도 휴직여부
					IF v_BJIK_YSNO2 > 0 THEN

						IF v_EXJA_CONT > 0 THEN
                           --평일 일수
							SELECT COUNT(A.CALN_DATE) INTO v_WORK_DCNT
							  FROM TM_CALDXM A
							 WHERE A.HOGB_CODE = '01'
							   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
													   AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD');

							SELECT COUNT(CASE WHEN A.EXDV_CODE IN ('00','08') THEN A.EXDV_CODE END) --육아휴직일수,가족돌봄휴직일수
								  ,COUNT(CASE WHEN A.EXDV_CODE <> '00' THEN A.EXDV_CODE END)  --일반휴직일수
							  INTO v_EXDV_DCNT_00, v_EXDV_DCNT
							  FROM (
								SELECT A.CALN_DATE, MAX(B.EXDV_CODE) AS EXDV_CODE
								  FROM TM_CALDXM A
								   INNER JOIN HR_EXJAIM B
									  ON A.CALN_DATE BETWEEN B.EXCP_BEDE AND NVL(B.EXCP_ENDD,'29991231')
									 AND B.EXCP_BEDE <= TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
									 AND NVL(B.EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
									 AND B.EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
									 AND SF_GET_REFXFILD('EXDV_CODE', B.EXDV_CODE, '1') = '1'
								 WHERE A.HOGB_CODE = '01'
								   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
													   AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
								 GROUP BY A.CALN_DATE
							 ) A;

							--출근율
							-- 유아휴직 1년차
							v_WORK_RATE_00 := CASE WHEN v_WORK_DCNT - v_EXDV_DCNT_00 = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / (v_WORK_DCNT - v_EXDV_DCNT_00) END;
							-- 일반휴직
							v_WORK_RATE := CASE WHEN v_WORK_DCNT = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / v_WORK_DCNT END;

							v_NEXT_OCUR_DCNT := CASE WHEN v_WORK_RATE >= 0.8 THEN v_NEXT_OCUR_DCNT
												WHEN v_WORK_RATE_00 >= 0.8 THEN v_NEXT_OCUR_DCNT - TRUNC(v_NEXT_OCUR_DCNT * (v_EXDV_DCNT_00 + v_EXDV_DCNT) / v_WORK_DCNT,0)
												ELSE 0 END;
						END IF;

						v_POCR_DCNT_TEMP := TRUNC(MONTHS_BETWEEN(TO_DATE(v_LAST_ANDT,'YYYYMMDD'),TO_DATE(v_EXCP_ENDD,'YYYYMMDD')))
										+ NVL(v_BEFORE_DCNT_TEMP,0)  ; --근무가능월수 체크
						v_NEXT_OCUR_DCNT_TEMP := TRUNC(v_NEXT_OCUR_DCNT_TEMP1/2,0); --원래 발생 연차에서 /2를 한 값을 계산

						--출근율이 80% 이상이면
						IF v_WORK_RATE_00 >= 0.8 THEN

							IF v_CBGB_CODE = '1' AND v_BJIK_LAST_ANDT > '20121231'
								AND v_NEXT_OCUR_DCNT_TEMP > v_NEXT_OCUR_DCNT THEN
								v_OCUR_DCNT := v_NEXT_OCUR_DCNT;
							ELSIF v_CBGB_CODE = '2' AND v_BJIK_LAST_ANDT > '20120801'
								AND v_NEXT_OCUR_DCNT_TEMP > v_NEXT_OCUR_DCNT THEN
								v_OCUR_DCNT := v_NEXT_OCUR_DCNT;
							ELSE
								v_OCUR_DCNT := v_NEXT_OCUR_DCNT_TEMP;

							END IF;

						ELSE
							--보험(1)/의료(2)구분
							IF v_CBGB_CODE = '1' THEN
								IF  v_CBGB_CODE = '1' AND v_POCR_DCNT_TEMP <= v_NEXT_OCUR_DCNT_TEMP AND v_BJIK_LAST_ANDT > '20121231' THEN
									v_OCUR_DCNT := v_POCR_DCNT_TEMP;
								ELSE
									v_OCUR_DCNT := v_NEXT_OCUR_DCNT_TEMP;
								END IF;
							ELSE
								IF v_CBGB_CODE = '2' AND v_POCR_DCNT_TEMP <= v_NEXT_OCUR_DCNT_TEMP AND v_BJIK_LAST_ANDT > '20120801' THEN

									v_OCUR_DCNT := v_POCR_DCNT_TEMP;


								ELSE

									v_OCUR_DCNT := v_NEXT_OCUR_DCNT_TEMP;

								END IF;

                            END IF;
                        END IF;

					 --선사용발생일수(v_POCR_DCNT) 해당연도부터 입사일까지 개월수가 2보다 크면 0
					ELSIF TRUNC(MONTHS_BETWEEN(TO_DATE(ar_STDS_DATE,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD'))/12,0) < 2 THEN
                      v_OCUR_DCNT := 0;

                    ELSE
                        SELECT COUNT(EMPL_NUMB)
						  INTO v_BJIK_YSNO
						  FROM HR_EXJAIM
						 WHERE NVL(EXCP_ENDD,'29991231') < v_LAST_ANDT
						   AND NVL(EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
						   AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
						   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

						--휴직기간에 따라 차감..

						--출근율 = 근무일수/평일,
						--1년차 육아휴직 출근율 = 근무일수-휴직일수/평일-휴직일수. 단, 연차발생일수 - (연차발생일수 * 휴직일수/평일).
						--출근율 80%이상 인경우 정상부여, 미만인경우 0.

						IF v_EXJA_CONT > 0 THEN
							--평일 일수
							SELECT COUNT(A.CALN_DATE) INTO v_WORK_DCNT
							  FROM TM_CALDXM A
							 WHERE A.HOGB_CODE = '01'
							   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
													   AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD');


							SELECT COUNT(CASE WHEN A.EXDV_CODE IN ('00','08') THEN A.EXDV_CODE END) --육아휴직일수, 가족돌봄휴직일수
								  ,COUNT(CASE WHEN A.EXDV_CODE <> '00' THEN A.EXDV_CODE END)  --일반휴직일수
							  INTO v_EXDV_DCNT_00, v_EXDV_DCNT
							  FROM (
								SELECT A.CALN_DATE, MAX(B.EXDV_CODE) AS EXDV_CODE
								  FROM TM_CALDXM A
								   INNER JOIN HR_EXJAIM B
									  ON A.CALN_DATE BETWEEN B.EXCP_BEDE AND NVL(B.EXCP_ENDD,'29991231')
									 AND B.EXCP_BEDE <= TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
									 AND NVL(B.EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
									 AND B.EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
									 AND SF_GET_REFXFILD('EXDV_CODE', B.EXDV_CODE, '1') = '1'
								 WHERE A.HOGB_CODE = '01'
								   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
													   AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
								 GROUP BY A.CALN_DATE
							 ) A;

							--출근율
							v_WORK_RATE_00 := CASE WHEN v_WORK_DCNT - v_EXDV_DCNT_00 = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / (v_WORK_DCNT - v_EXDV_DCNT_00) END;
							v_WORK_RATE := CASE WHEN v_WORK_DCNT = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / v_WORK_DCNT END;

							v_NEXT_OCUR_DCNT := CASE WHEN v_WORK_RATE >= 0.8 THEN v_NEXT_OCUR_DCNT
												WHEN v_WORK_RATE_00 >= 0.8 THEN v_NEXT_OCUR_DCNT - TRUNC(v_NEXT_OCUR_DCNT * (v_EXDV_DCNT_00 + v_EXDV_DCNT) / v_WORK_DCNT,0)
												ELSE v_NEXT_OCUR_DCNT END;
						END IF;

						v_OCUR_DCNT := TRUNC(v_NEXT_OCUR_DCNT / 2,0);

					END IF;
                ELSE
                   v_GUBUN := 4;  --구분
    --  <-- 요기까지 23
                     v_OCUR_DCNT :=  0;
                    --v_OCUR_DCNT :=  v_BEFORE_DCNT_TEMP;  여기 주석풀면 신화진 해결!!!
                END IF;
          --연차발생일
--            v_OCUR_DCNT := CASE WHEN v_WORK_RATE >= 0.8 THEN v_OCUR_DCNT
--                                WHEN v_WORK_RATE_00 >= 0.8 THEN v_OCUR_DCNT - TRUNC(v_OCUR_DCNT * (v_EXDV_DCNT_00 + v_EXDV_DCNT) / v_WORK_DCNT,0)
--                                ELSE 0 END;
            END IF;
        END IF;
        /********* 연차발생일수(근속제외가 있는 경우) ***********/


--        v_OCUR_DCNT := v_OCUR_DCNT - TRUNC(v_OCUR_DCNT * CASE WHEN TRUNC(v_EXDV_DCNT_00,2) <= 0.2 THEN 0 ELSE v_EXDV_DCNT_00 END,0) - TRUNC(CASE WHEN TRUNC(v_EXDV_DCNT,2) <= 0.2 THEN 0 ELSE v_OCUR_DCNT - TRUNC(v_OCUR_DCNT * CASE WHEN TRUNC(v_EXDV_DCNT_00,2) <= 0.2 THEN 0 ELSE v_EXDV_DCNT_00 END,0) END,0); --연차발생일수
--        v_OCUR_DCNT := v_OCUR_DCNT - TRUNC(v_OCUR_DCNT * CASE WHEN v_WORK_RATE_00 <= 0.2 THEN 0 ELSE v_EXDV_DCNT_00 END,0) - TRUNC(CASE WHEN v_EXDV_DCNT <= 0.2 THEN 0 ELSE v_OCUR_DCNT - TRUNC(v_OCUR_DCNT * CASE WHEN v_WORK_RATE_00 <= 0.2 THEN 0 ELSE v_EXDV_DCNT_00 END,0) END,0); --연차발생일수
        --추가발생일수(v_AOCU_DCNT)
        --보험사업
        --IF v_CBGB_CODE = '1' THEN

		/********* 연차보상발생일수 ***********/

		IF v_CONT_YEAR = 3 THEN
			--3년차의 경우 만1년시점에 발생한 연차갯수 + 월수에 비례하여 추가
			v_AOCU_DCNT := CEIL((TO_DATE(v_ANSD_DATE,'YYYYMMDD') - TO_DATE(v_PCOP_JCDT,'YYYYMMDD') ) /365 * 15);
		END IF;
        /********* 연차보상발생일수 ***********/

		/********** 근속제외정보가 등록된 경우(기관상관없이) 추가 또는 이월된 데이터 **************/
		IF v_EXJA_CONT > 0 THEN
			--휴직전 연차 남은 갯수
			BEGIN
				--연차추가
				SELECT NVL(AOCU_DCNT,0)
				  INTO v_IWOL_CNT1
				  FROM HR_EXJAIM
				 WHERE EXCP_BEDE < v_LAST_ANDT
				   AND EXCP_ENDD >= v_LAST_ANDT
				   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1'
				   AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB = v_RERN_NUMB)
				 ;

			EXCEPTION
				WHEN NO_DATA_FOUND THEN
				   v_IWOL_CNT1 :=  0;
				WHEN OTHERS THEN
				   v_IWOL_CNT1 :=  0;
			END;

			BEGIN
				--연차이월
				SELECT NVL(SUM(IWOL_DCNT),0)
				  INTO v_IWOL_CNT2
				  FROM HR_EXJAIM
				 WHERE EXCP_ENDD < v_LAST_ANDT
				   AND EXCP_ENDD >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12),'YYYYMMDD')
				   AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1'
				   AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB = v_RERN_NUMB)
				 ;
			EXCEPTION
				WHEN NO_DATA_FOUND THEN
				   v_IWOL_CNT2 :=  0;
				WHEN OTHERS THEN
				   v_IWOL_CNT2 :=  0;
			END;
		ELSE
			v_IWOL_CNT1 := 0;
			v_IWOL_CNT2 := 0;

		END IF;

		/********** 근속제외정보가 등록된 경우(기관상관없이) 추가 또는 이월된 데이터 **************/

		/********* 연차보상발생일수(계산) ***********/
		v_AOCU_DCNT := v_AOCU_DCNT + v_IWOL_CNT1 + v_IWOL_CNT2;
        /********* 연차보상발생일수(계산) ***********/

		/********* 연차조정일수 ***********/
        --SV_HOLIAD(조정일수 반영)
        BEGIN
            SELECT NVL(AOCU_DCNT,0),
                   NVL(AUSE_DCNT,0)
              INTO v_ADJT_DCNT ,
                   v_AUSE_DCNT --MHS 2015.3.19 사용(조정) 추가  인사부 박성민 과장 요청 ( 잔여일수에서 추가로 빼준다)
              FROM SV_HOLIAD --연차휴가 추가 발생일수
             WHERE EMPL_NUMB = ar_EMPL_NUMB
               AND STDS_DATE <= ar_STDS_DATE
               AND ANNU_TMUS >= ar_STDS_DATE
               ;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
               v_ADJT_DCNT :=  0;
               v_AUSE_DCNT :=  0;
            WHEN OTHERS THEN
               v_ADJT_DCNT :=  0;
               v_AUSE_DCNT :=  0;
        END;
		/********* 연차조정일수 ***********/

		/********* 연차보상발생일수(최종계산) ***********/
        v_AOCU_DCNT := v_AOCU_DCNT + v_ADJT_DCNT;
		/********* 연차보상발생일수(계산) ***********/

		/********* 사용일수 조회 ***********/
		--선사용일수 차감..   (전년도 선사용일수 조회)
		SELECT NVL(SUM(CASE WHEN A.SLGB_CODE IN ('03010') THEN 1 WHEN A.SLGB_CODE IN ('03021','03022') THEN 0.5 ELSE DGNL_TIME/NVL(TO_NUMBER(SUBSTR(WKDA_TIMI,0,2)),8) END),0)
		  INTO v_PUSE_DCNT
		  FROM SV_DGNLXM A
		   INNER JOIN TM_CALDXM B
			  ON B.CALN_DATE BETWEEN SUBSTR(A.DGBG_TIME,1,8) AND SUBSTR(A.DGED_TIME,1,8)
			 AND (v_CBGB_CODE = '2' OR B.HOGB_CODE = '01')
		   INNER JOIN HR_MASTXM C
			  ON A.EMPL_NUMB = C.EMPL_NUMB
			LEFT OUTER JOIN SV_JIKDXM D
			  ON C.JBGP_CODE = D.JBGP_CODE
			 AND C.CPOS_CODE = D.CPOS_CODE
		 WHERE C.RERN_NUMB = v_RERN_NUMB
		   AND C.PCOP_JCDT =  v_PCOP_JCDT AND C.ANSD_DATE = v_ANSD_DATE_W
		   AND A.DGNL_CODE = '030'
		   AND A.APDS_CODE = '132' --결재완료
		   AND A.AMCD_CODE = '01'
		   AND NVL(A.DELE_YSNO,' ') <> '1'
		   AND B.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
							   AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD');

        -- 결근, 무급휴가 차감
   /*     SELECT NVL(SUM(DGNL_DCNT), 0) AS DGNL_DCNT
          INTO v_DGNL_DCNT
          FROM (
                SELECT distinct A.DGNL_NUMB, A.DGNL_DCNT
                  FROM SV_DGNLXM A
                   INNER JOIN TM_CALDXM B
                        ON B.CALN_DATE BETWEEN SUBSTR(A.DGBG_TIME,1,8) AND SUBSTR(A.DGED_TIME,1,8)
                       AND (v_CBGB_CODE = '2' OR B.HOGB_CODE = '01')
                   INNER JOIN HR_MASTXM C
                        ON A.EMPL_NUMB = C.EMPL_NUMB
                   LEFT OUTER JOIN SV_JIKDXM D
                        ON C.JBGP_CODE = D.JBGP_CODE
                       AND C.CPOS_CODE = D.CPOS_CODE
                 WHERE C.RERN_NUMB = v_RERN_NUMB
                   AND C.PCOP_JCDT =  v_PCOP_JCDT AND C.ANSD_DATE = v_ANSD_DATE_W
                   AND A.DGNL_CODE IN ('110', '180')
                   AND A.APDS_CODE = '132' --결재완료
                   AND A.AMCD_CODE = '01'
                   AND NVL(A.DELE_YSNO,' ') <> '1'
                   AND B.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                               AND TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
              );
      */

        --사용일수(v_USEX_DCNT), 선사용일수(v_PUSX_DCNT)
        IF v_CONT_YEAR < 1 THEN
            v_USEX_DCNT := 0;
            v_PUSX_DCNT := 0;

			--DGNL_CODE : 근태코드
			--SLGB_CODE : 근태세부코드
			-

        ELSIF v_CONT_YEAR <= 3 THEN
            --1,2,3년차의 경우 입사일 부터 기준일까지
            SELECT NVL(SUM(CASE WHEN A.DGNL_CODE = '020' AND A.SLGB_CODE = '02010' THEN 1
                                WHEN A.DGNL_CODE = '020' AND A.SLGB_CODE IN ('02020','02021','02022') THEN 0.5
                                WHEN A.DGNL_CODE = '020' THEN DGNL_TIME/NVL(TO_NUMBER(SUBSTR(WKDA_TIMI,0,2)),8) ELSE 0 END),0)
                  ,NVL(SUM(CASE WHEN A.DGNL_CODE = '030' AND A.SLGB_CODE = '03010' THEN 1 WHEN A.DGNL_CODE = '030' AND A.SLGB_CODE IN ('03021','03022') THEN 0.5 WHEN A.DGNL_CODE = '030' THEN DGNL_TIME/NVL(TO_NUMBER(SUBSTR(WKDA_TIMI,0,2)),8) ELSE 0 END),0)
                  ,NVL(SUM(CASE WHEN A.SLGB_CODE = '10020' THEN 1  ELSE 0 END),0)
                  ,NVL(SUM(CASE WHEN A.SLGB_CODE IN ('12030','13030','14030','12040','13040','14040')  THEN DGNL_TIME/NVL(TO_NUMBER(SUBSTR(WKDA_TIMI,0,2)),8) ELSE 0 END), 0)
              INTO v_USEX_DCNT, v_PUSX_DCNT, v_AUSX_DCNT, v_USEA_DCNT
              FROM SV_DGNLXM A
               INNER JOIN TM_CALDXM B
                  ON B.CALN_DATE BETWEEN SUBSTR(A.DGBG_TIME,1,8) AND SUBSTR(A.DGED_TIME,1,8)
                 AND (v_CBGB_CODE = '2' OR B.HOGB_CODE = '01')
               INNER JOIN HR_MASTXM C
                  ON A.EMPL_NUMB = C.EMPL_NUMB
                LEFT OUTER JOIN SV_JIKDXM D
                  ON C.JBGP_CODE = D.JBGP_CODE
                 AND C.CPOS_CODE = D.CPOS_CODE
             WHERE C.RERN_NUMB = v_RERN_NUMB
               AND C.PCOP_JCDT =  v_PCOP_JCDT AND C.ANSD_DATE = v_ANSD_DATE_W
               AND (A.DGNL_CODE IN ('020', '030') OR A.SLGB_CODE IN ('10020','12030','13030','14030','12040','13040','14040'))
               AND A.DGNL_CODE <> '900' --SJY 2018.06.27 취소가 합산되지 않도록 수정
               AND A.APDS_CODE = '132' --결재완료
               AND A.AMCD_CODE = '01'
               AND NVL(A.DELE_YSNO,' ') <> '1'
               AND B.CALN_DATE BETWEEN v_PCOP_JCDT AND DECODE(ar_USED_TYPE, '0', LEAST(ar_STDS_DATE, TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 24)-1,'YYYYMMDD')), TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 24)-1,'YYYYMMDD'));

        ELSIF v_CONT_YEAR > 3 THEN

            SELECT NVL(SUM(CASE WHEN A.DGNL_CODE = '020' AND A.SLGB_CODE = '02010' THEN 1
                                WHEN A.DGNL_CODE = '020' AND A.SLGB_CODE IN ('02020','02021','02022') THEN 0.5
                                WHEN A.DGNL_CODE = '020' THEN DGNL_TIME/NVL(TO_NUMBER(SUBSTR(WKDA_TIMI,0,2)),8)
                           ELSE 0
                           END),0)
                  ,NVL(SUM(CASE WHEN A.DGNL_CODE = '030' AND A.SLGB_CODE = '03010' THEN 1 WHEN A.DGNL_CODE = '030' AND A.SLGB_CODE IN ('03021','03022') THEN 0.5 WHEN A.DGNL_CODE = '030' THEN DGNL_TIME/NVL(TO_NUMBER(SUBSTR(WKDA_TIMI,0,2)),8) ELSE 0 END),0)
                  ,NVL(SUM(CASE WHEN A.SLGB_CODE = '10020' THEN 1  ELSE 0 END),0)
                  ,NVL(SUM(CASE WHEN A.SLGB_CODE IN ('12030','13030','14030','12040','13040','14040')  THEN DGNL_TIME/NVL(TO_NUMBER(SUBSTR(WKDA_TIMI,0,2)),8) ELSE 0 END), 0)
              INTO v_USEX_DCNT, v_PUSX_DCNT, v_AUSX_DCNT, v_USEA_DCNT
              FROM SV_DGNLXM A
               INNER JOIN TM_CALDXM B
                  ON B.CALN_DATE BETWEEN SUBSTR(A.DGBG_TIME,1,8) AND SUBSTR(A.DGED_TIME,1,8)
                 AND (v_CBGB_CODE = '2' OR B.HOGB_CODE = '01')
               INNER JOIN HR_MASTXM C
                  ON A.EMPL_NUMB = C.EMPL_NUMB
                LEFT OUTER JOIN SV_JIKDXM D
                  ON C.JBGP_CODE = D.JBGP_CODE
                 AND C.CPOS_CODE = D.CPOS_CODE
             WHERE C.RERN_NUMB = v_RERN_NUMB
               AND C.PCOP_JCDT =  v_PCOP_JCDT AND C.ANSD_DATE = v_ANSD_DATE_W
               AND (A.DGNL_CODE IN ('020', '030') OR A.SLGB_CODE IN ('10020','12030','13030','14030','12040','13040','14040'))
               AND A.DGNL_CODE <> '900' --SJY 2018.06.27 취소가 합산되지 않도록 수정
               AND A.APDS_CODE = '132' --결재완료
               AND A.AMCD_CODE = '01'
               AND NVL(A.DELE_YSNO,' ') <> '1'
               AND B.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 12 * (v_CONT_YEAR-2)),'YYYYMMDD') AND DECODE(ar_USED_TYPE, '0', LEAST(ar_STDS_DATE, TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 12 * (v_CONT_YEAR-1))-1,'YYYYMMDD')),TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 12 * (v_CONT_YEAR-1))-1,'YYYYMMDD'));






--IF  ar_EMPL_NUMB = '4446' THEN
--IF  ar_EMPL_NUMB = '5445' THEN
--
--     --MHS 2015.1.28 연차 기준자료 확인
--     INSERT INTO  MHS_YEONCH_DATA (
--        EMPL_NUMB ,
--        STDS_DATE ,
--        PCOP_JCDT ,  --공단입사일
--        ANSD_DATE ,  --연차기준일
--        CBGB_CODE ,  --보험/의료사업구분
--        CONT_YEAR ,  --근무년차
--        LAST_ANDT ,  --최근연차기준일
--        NEXT_ANDT ,  --다음연차기준일
--        SU1_DATE  ,  --선사용기준일(시작)
--        SU2_DATE  ,  --선사용기준일(종료)
--        US1_DATE  ,  --사용기준일(시작)
--        US2_DATE  ,  --사용기준일(종료)
--        OCUR_DCNT ,  --연차발생일수
--        AOCU_DCNT ,  --추가발생일수
--        PUSE_DCNT ,  --선사용차감일수
--        USEX_DCNT ,  --사용일수
--        INST_DATE ,
--        BIGO
--        ) VALUES (
--            ar_EMPL_NUMB,
--            ar_STDS_DATE,
--            v_PCOP_JCDT,   --공단입사일
--            v_ANSD_DATE,  --연차기준일
--            v_CBGB_CODE,   --보험/의료사업구분
--            v_CONT_YEAR,  --근무연차
--            v_LAST_ANDT ,  --최근연차기준일
--            v_NEXT_ANDT ,  --다음연차기준일
--            TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD') , --선사용기준일(시작)
--            TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD'),                --선사용기준일(종료)
--            TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 12 * (v_CONT_YEAR-2)),'YYYYMMDD') , --사용기준일(시작)
--            LEAST(ar_STDS_DATE, TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 12 * (v_CONT_YEAR-1))-1,'YYYYMMDD')) ,  --사용기준일(종료)
--            v_OCUR_DCNT,   --연차발생일수
--            v_AOCU_DCNT,   --추가발생일수
--            v_PUSE_DCNT,   --선사용차감일수
--            v_USEX_DCNT,   --사용일수
--            SYSDATE ,
--           --'ar_USED_TYPE:: '||ar_USED_TYPE||' v_GUBUN:: '||v_GUBUN||'v_ORGC_DCNT:: '||v_ORGC_DCNT||'v_EXJA_CONT :: ' ||  v_WORK_DCNT || 'v_EXDV_DCNT_00 :: ' || v_EXDV_DCNT_00 || 'v_EXDV_DCNT :: ' || v_EXDV_DCNT ||  'v_WORK_RATE_00 :: ' || v_WORK_RATE_00 ||  'v_WORK_RATE :: ' || v_WORK_RATE ||  'v_EXCP_DATE :: ' || v_EXCP_DATE || 'v_BJIK_LAST_ANDT :: ' || v_BJIK_LAST_ANDT || 'v_EXCP_BEDE1 :: ' || v_EXCP_BEDE1 || 'v_EXCP_ENDD1 :: ' || v_EXCP_ENDD1
--           'v_RERN_NUMB:: '||v_RERN_NUMB||' v_PCOP_JCDT:: '||v_PCOP_JCDT||' v_ANSD_DATE:: '||v_ANSD_DATE
--        );
--END IF;

        END IF;

        --v_USEX_DCNT := v_USEX_DCNT - TRUNC(v_USEA_DCNT);
        v_USEX_DCNT := v_USEX_DCNT + TRUNC(v_USEA_DCNT);
        /********* 사용일수 조회 ***********/
        --MHS 2015.4.21 사용일수 - (근무상황부에서 외출,조퇴,지참이 누적 8시간이 되면 연차휴가에서 1일이 공제되어야 하는데요.. 인사부 박성민 과장님)
        /*
        IF v_USEX_DCNT < 0 THEN
           v_USEX_DCNT := ABS(v_USEX_DCNT);
        END IF;
        */

        /***************************************** 선연차 시작 *****************************************************/
        ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

        --최근연차기준일과 기준년도 휴직시작일
        SELECT MAX(EXCP_BEDE)
          INTO v_EXCP_BEDE1
          FROM HR_EXJAIM
         WHERE NVL(EXCP_BEDE,'29991231') >=v_LAST_ANDT
           AND NVL(EXCP_BEDE,'29991231') < TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), 12), 'YYYYMMDD')
           AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
           AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

     --; select * From  HR_EXJAIM where  EMPL_NUMB = '30308';

        --최근연차기준일과 기준년도 휴직시작일 사이의 휴직종료일
        SELECT MAX(EXCP_ENDD)
          INTO v_EXCP_ENDD1
          FROM HR_EXJAIM
         WHERE NVL(EXCP_ENDD,'29991231') >= v_LAST_ANDT
           AND NVL(EXCP_ENDD,'29991231') < v_EXCP_BEDE1
           AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
           AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1'
           ;


	     SELECT CASE WHEN v_EXCP_ENDD1 IS NULL THEN
							TRUNC(MONTHS_BETWEEN(TO_DATE(v_EXCP_BEDE1,'YYYYMMDD'),TO_DATE(v_LAST_ANDT,'YYYYMMDD')))  --근무가능월수 체크
				   WHEN v_EXCP_ENDD1 IS NOT NULL THEN
							TRUNC(MONTHS_BETWEEN(TO_DATE(v_EXCP_BEDE1,'YYYYMMDD'),TO_DATE(v_EXCP_ENDD1,'YYYYMMDD')))  --근무가능월수 체크
			   END
	     INTO v_BEFORE_DCNT_TEMP
	     FROM DUAL;

        ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

        --당해복직자는 선사용 0
        --당해 복직자는 선사용 0일에서 그 다음 연차기준일까지 만근가능 개월을 계산하여 미리 선사용 가능일수로 넣어준다.
        --선사용 가능 일수를 넣어줄때 계산은 만근가능 개월수와 (원래 연차 발생일/2)를 한 일수를(나머지는 버림) 계산하여 작은 것으로 넣어준다.
        --기준일자는 보험 사업의 경우 다음연차 기준일이 2014.01.01부터 된다. 의료사업은 다음연차 기준일이 2013.08.02부터 가능하다.

        -- EY! 2016.7.26. 복직일이 1.1.인 경우에 대한 올바른 값 조회를 위해 수정 EXCP_ENDD -> TO_CHAR(TO_DATE(EXCP_ENDD, 'YYYYMMDD') + 1, 'YYYYMMDD')
        SELECT COUNT(EMPL_NUMB)
          INTO v_BJIK_YSNO2
          FROM HR_EXJAIM
         WHERE NVL(TO_CHAR(TO_DATE(EXCP_ENDD, 'YYYYMMDD') + 1, 'YYYYMMDD'),'29991231') >= v_LAST_ANDT
           AND NVL(TO_CHAR(TO_DATE(EXCP_ENDD, 'YYYYMMDD') + 1, 'YYYYMMDD'),'29991231') < TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), 12), 'YYYYMMDD')
           AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
           AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';
        -- EY! 끝!


        --발령종료일
        SELECT MAX(EXCP_ENDD)
		  INTO v_EXCP_ENDD
	     FROM HR_EXJAIM
--         WHERE NVL(EXCP_ENDD,'29991231') >= v_LAST_ANDT
--           AND NVL(EXCP_ENDD,'29991231') < TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), 12), 'YYYYMMDD')
--           AND
	     WHERE EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
	       AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

		--다음연차기준일 기준 발생일수
		v_NEXT_CONT_YEAR := CEIL(MONTHS_BETWEEN(TO_DATE(v_NEXT_ANDT,'YYYYMMDD') + 1,TO_DATE(v_ANSD_DATE,'YYYYMMDD'))/12) + 1;

		IF v_NEXT_CONT_YEAR < 1 THEN
			v_NEXT_OCUR_DCNT := 0;
		ELSIF v_NEXT_CONT_YEAR = 1 THEN
			--1년차의 경우 매월 입사일자에 1개씩 발생
			v_NEXT_OCUR_DCNT := TRUNC(MONTHS_BETWEEN(TO_DATE(v_NEXT_ANDT,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD')),0);
		ELSIF v_NEXT_CONT_YEAR = 2 THEN
			--근속년수 구하기
			IF TRUNC(MONTHS_BETWEEN(TO_DATE(v_NEXT_ANDT,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD'))/12,0) < 1 THEN
				--2년차의 경우 만1년이 되기까지는 매월 1개씩 발생
				v_NEXT_OCUR_DCNT := TRUNC(MONTHS_BETWEEN(TO_DATE(v_NEXT_ANDT,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD')),0);
			ELSE
				--만1년이 도래한 경우 15개 발생
				IF v_PCOP_JCDT < '20170530' THEN --SJY 2018.05.28
				   v_NEXT_OCUR_DCNT := 15;
				ELSE
					v_NEXT_OCUR_DCNT := 26;   --2018.05.25 SJY
				END IF;

			END IF;
		ELSIF v_NEXT_CONT_YEAR = 3 THEN
			--3년차의 경우 만1년시점에 발생한 연차갯수 + 월수에 비례하여 추가
				IF v_PCOP_JCDT < '20170530' THEN --SJY 2018.05.28
				   v_NEXT_OCUR_DCNT := 15;
				ELSE
				   v_NEXT_OCUR_DCNT := 26;  --2018.05.25 SJY
				END IF;

		ELSIF v_NEXT_CONT_YEAR >= 4 THEN
			v_NEXT_OCUR_DCNT := 15 + TRUNC((v_NEXT_CONT_YEAR-3)/2);
			IF v_NEXT_OCUR_DCNT > 25 THEN
				v_NEXT_OCUR_DCNT := 25;
			END IF;
		END IF;


        v_NEXT_OCUR_DCNT_TEMP1 := v_NEXT_OCUR_DCNT;



       -- 전년도 휴직여부
        IF v_BJIK_YSNO2 > 0 THEN
            IF v_EXJA_CONT > 0 THEN
				--평일 일수
                SELECT COUNT(A.CALN_DATE) INTO v_WORK_DCNT
                  FROM TM_CALDXM A
                 WHERE A.HOGB_CODE = '01'
                   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                                           AND TO_CHAR(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD');

                SELECT COUNT(CASE WHEN A.EXDV_CODE IN ('00','08') THEN A.EXDV_CODE END) --육아휴직일수, 가족돌봄휴직일수
                      ,COUNT(CASE WHEN A.EXDV_CODE <> '00' THEN A.EXDV_CODE END)  --일반휴직일수
                  INTO v_EXDV_DCNT_00, v_EXDV_DCNT
                  FROM (
                    SELECT A.CALN_DATE, MAX(B.EXDV_CODE) AS EXDV_CODE
                      FROM TM_CALDXM A
                       INNER JOIN HR_EXJAIM B
                          ON A.CALN_DATE BETWEEN B.EXCP_BEDE AND NVL(B.EXCP_ENDD,'29991231')
                         AND B.EXCP_BEDE <= TO_CHAR(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                         AND NVL(B.EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                         AND B.EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
                         AND SF_GET_REFXFILD('EXDV_CODE', B.EXDV_CODE, '1') = '1'
                     WHERE A.HOGB_CODE = '01'
                       AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                                           AND TO_CHAR(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                     GROUP BY A.CALN_DATE
                 ) A;

                --출근율
                -- 유아휴직 1년차
                v_WORK_RATE_00 := CASE WHEN v_WORK_DCNT - v_EXDV_DCNT_00 = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / (v_WORK_DCNT - v_EXDV_DCNT_00) END;
                -- 일반휴직
                v_WORK_RATE := CASE WHEN v_WORK_DCNT = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / v_WORK_DCNT END;

                v_NEXT_OCUR_DCNT := CASE WHEN v_WORK_RATE >= 0.8 THEN
											v_NEXT_OCUR_DCNT
										WHEN v_WORK_RATE_00 >= 0.8 THEN
											v_NEXT_OCUR_DCNT - TRUNC(v_NEXT_OCUR_DCNT * (v_EXDV_DCNT_00 + v_EXDV_DCNT) / v_WORK_DCNT,0)
										ELSE 0
									END;
            END IF;

            v_POCR_DCNT_TEMP := TRUNC(MONTHS_BETWEEN(
														TO_DATE(v_NEXT_ANDT,'YYYYMMDD')
													   ,TO_DATE(v_EXCP_ENDD,'YYYYMMDD')
													 ))
								 + NVL(v_BEFORE_DCNT_TEMP,0)  ; --근무가능월수 체크

            v_NEXT_OCUR_DCNT_TEMP := TRUNC(v_NEXT_OCUR_DCNT_TEMP1/2,0); --원래 발생 연차에서 /2를 한 값을 계산

            --v_WORK_RATE_00_TEMP := v_NEXT_OCUR_DCNT - TRUNC(v_NEXT_OCUR_DCNT * (v_EXDV_DCNT_00 + v_EXDV_DCNT) / v_WORK_DCNT,0);  --육아 휴직 1년차의 경우

            --출근율이 80% 이상이면
            IF v_WORK_RATE_00 >= 0.8 THEN

                IF v_CBGB_CODE = '1' AND v_LAST_ANDT > '20121231' AND v_NEXT_OCUR_DCNT_TEMP > v_NEXT_OCUR_DCNT THEN

                    v_POCR_DCNT := v_NEXT_OCUR_DCNT;

                ELSIF v_CBGB_CODE = '2' AND v_LAST_ANDT > '20120801' AND v_NEXT_OCUR_DCNT_TEMP > v_NEXT_OCUR_DCNT THEN

                    v_POCR_DCNT := v_NEXT_OCUR_DCNT;

                ELSE

                    v_POCR_DCNT := v_NEXT_OCUR_DCNT_TEMP;

                END IF;

            ELSE

                IF  v_POCR_DCNT_TEMP > 0  THEN
                 --보험(1)/의료(2)구분
                    IF v_CBGB_CODE = '1'  THEN

                        IF  v_CBGB_CODE = '1' AND v_POCR_DCNT_TEMP <= v_NEXT_OCUR_DCNT_TEMP AND v_LAST_ANDT > '20121231' THEN

                            v_POCR_DCNT := v_POCR_DCNT_TEMP;

                           -- v_OCUR_DCNT := v_POCR_DCNT_TEMP;

                        ELSE

                            v_POCR_DCNT := v_NEXT_OCUR_DCNT_TEMP;

                           -- v_OCUR_DCNT := v_NEXT_OCUR_DCNT_TEMP;

                        END IF;

                    ELSE

                        IF v_CBGB_CODE = '2'   AND v_POCR_DCNT_TEMP <= v_NEXT_OCUR_DCNT_TEMP AND v_LAST_ANDT > '20120801' THEN

                            v_POCR_DCNT := v_POCR_DCNT_TEMP;

                         --   v_OCUR_DCNT := v_POCR_DCNT_TEMP;

                        ELSE

                            v_POCR_DCNT := v_NEXT_OCUR_DCNT_TEMP;

                         --   v_OCUR_DCNT := v_NEXT_OCUR_DCNT_TEMP;

                        END IF;

                    END IF;

				ELSE

					v_POCR_DCNT := 0;

				END IF;


            /*
            IF v_CBGB_CODE = '1' AND v_POCR_DCNT_TEMP <= v_NEXT_OCUR_DCNT_TEMP AND v_LAST_ANDT > '20121231' THEN

               v_POCR_DCNT := v_POCR_DCNT_TEMP;

            ELSE
            */


			END IF;

            /*
            IF v_CBGB_CODE = '2' AND v_POCR_DCNT_TEMP <= v_NEXT_OCUR_DCNT_TEMP AND v_LAST_ANDT > '20120801' THEN

                v_POCR_DCNT := v_POCR_DCNT_TEMP;
            ELSE
                v_POCR_DCNT := v_NEXT_OCUR_DCNT_TEMP;
            END IF;
            */

  /*
        IF v_BJIK_YSNO2 > 0 THEN
            v_POCR_DCNT := 0;
*/
         --선사용발생일수(v_POCR_DCNT) 해당연도부터 입사일까지 개월수가 2보다 크면 0
        ELSIF TRUNC(MONTHS_BETWEEN(TO_DATE(ar_STDS_DATE,'YYYYMMDD'),TO_DATE(v_PCOP_JCDT,'YYYYMMDD'))/12,0) < 2 THEN
            v_POCR_DCNT := 0;

        ELSE
            SELECT COUNT(EMPL_NUMB)
              INTO v_BJIK_YSNO       -- 전년도 복직여부
              FROM HR_EXJAIM
             WHERE NVL(EXCP_ENDD,'29991231') < v_LAST_ANDT
               AND NVL(EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
               AND EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
               AND SF_GET_REFXFILD('EXDV_CODE', EXDV_CODE, '1') = '1';

            --휴직기간에 따라 차감..

            --출근율 = 근무일수/평일,
            --1년차 육아휴직 출근율 = 근무일수-휴직일수/평일-휴직일수. 단, 연차발생일수 - (연차발생일수 * 휴직일수/평일).
            --출근율 80%이상 인경우 정상부여, 미만인경우 0.

            IF v_EXJA_CONT > 0 THEN
                --평일 일수
                SELECT COUNT(A.CALN_DATE) INTO v_WORK_DCNT
                  FROM TM_CALDXM A
                 WHERE A.HOGB_CODE = '01'
                   AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                                           AND TO_CHAR(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD');


                SELECT COUNT(CASE WHEN A.EXDV_CODE IN ('00','08') THEN A.EXDV_CODE END) --육아휴직일수, 가족돌봄휴직일수
                      ,COUNT(CASE WHEN A.EXDV_CODE <> '00' THEN A.EXDV_CODE END)  --일반휴직일수
                  INTO v_EXDV_DCNT_00, v_EXDV_DCNT
                  FROM (
                    SELECT A.CALN_DATE, MAX(B.EXDV_CODE) AS EXDV_CODE
                      FROM TM_CALDXM A
                       INNER JOIN HR_EXJAIM B
                          ON A.CALN_DATE BETWEEN B.EXCP_BEDE AND NVL(B.EXCP_ENDD,'29991231')
                         AND B.EXCP_BEDE <= TO_CHAR(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                         AND NVL(B.EXCP_ENDD,'29991231') >= TO_CHAR(ADD_MONTHS(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                         AND B.EMPL_NUMB IN (SELECT EMPL_NUMB FROM HR_MASTXM WHERE RERN_NUMB =  v_RERN_NUMB AND PCOP_JCDT =  v_PCOP_JCDT AND ANSD_DATE = v_ANSD_DATE_W)
                         AND SF_GET_REFXFILD('EXDV_CODE', B.EXDV_CODE, '1') = '1'
                     WHERE A.HOGB_CODE = '01'
                       AND A.CALN_DATE BETWEEN TO_CHAR(ADD_MONTHS(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD')
                                           AND TO_CHAR(TO_DATE(v_NEXT_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD')
                     GROUP BY A.CALN_DATE
                 ) A;

                --출근율
                v_WORK_RATE_00 := CASE WHEN v_WORK_DCNT - v_EXDV_DCNT_00 = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / (v_WORK_DCNT - v_EXDV_DCNT_00) END;
                v_WORK_RATE := CASE WHEN v_WORK_DCNT = 0 THEN 0 ELSE (v_WORK_DCNT - v_EXDV_DCNT_00 - v_EXDV_DCNT) / v_WORK_DCNT END;

                v_NEXT_OCUR_DCNT := CASE WHEN v_WORK_RATE >= 0.8 THEN v_NEXT_OCUR_DCNT
                                    WHEN v_WORK_RATE_00 >= 0.8 THEN v_NEXT_OCUR_DCNT - TRUNC(v_NEXT_OCUR_DCNT * (v_EXDV_DCNT_00 + v_EXDV_DCNT) / v_WORK_DCNT,0)
                                    ELSE v_NEXT_OCUR_DCNT END;
            END IF;


/*
            IF v_BJIK_YSNO > 0 THEN --휴직후 복직으로 인해 연차갯수 줄어든 경우
                v_POCR_DCNT := TRUNC(v_NEXT_OCUR_DCNT / 2,0);
            ELSE
                v_POCR_DCNT := TRUNC(v_NEXT_OCUR_DCNT / 3,0);
            END IF;
*/
            v_POCR_DCNT := TRUNC(v_NEXT_OCUR_DCNT / 2,0);

        END IF;



        IF v_OCUR_DCNT + v_AOCU_DCNT - v_PUSE_DCNT - v_USEX_DCNT - v_AUSE_DCNT = 0 THEN
            v_ACUR_DCNT := 1-v_AUSX_DCNT;
        ELSE
            v_ACUR_DCNT := 0;
        END IF;

    END;


 --IF  ar_EMPL_NUMB = '27089' THEN
 IF  ar_EMPL_NUMB = '5309'  THEN


    DELETE FROM KSK_TEMP;


    INSERT INTO KSK_TEMP
    (
         EMPL_NUMB     --사원번호
        ,v_ANSD_DATE   --연차기준일
        ,v_OCUR_DCNT   --연차발생일수
        ,v_AOCU_DCNT   --추가발생일수
        ,v_PUSE_DCNT   --선사용차감일수
        ,v_USEX_DCNT   --사용일수
        ,v_USEP_DCNT   --사용가능일수
        ,v_POCR_DCNT  --선사용발생일수
        ,v_PUSX_DCNT  --선사용사용일수
        ,v_PUSX_DCNT2 --선사용잔여일수
        ,v_ACUR_DCNT  --대체휴가 사용가능일수
        ,v_ORGC_DCNT  --원 발생일수
        ,v_USE_GIHAN  --사용기한
        ,BJIK_YSNO
        ,BJIK_YSNO2
        ,INST_DATE

    )
    VALUES
    (
         ar_EMPL_NUMB
        ,CASE WHEN v_PCOP_JCDT IS NULL OR v_ANSD_DATE IS NULL OR ar_STDS_DATE < v_PCOP_JCDT THEN '' ELSE CASE WHEN TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'),12),'YYYYMMDD') > ar_STDS_DATE THEN v_PCOP_JCDT ELSE v_ANSD_DATE END END   --연차기준일
        ,v_OCUR_DCNT   --연차발생일수
        ,v_AOCU_DCNT   --추가발생일수
        ,v_PUSE_DCNT   --선사용차감일수
        ,v_USEX_DCNT   --사용일수
        ,v_OCUR_DCNT + v_AOCU_DCNT - v_PUSE_DCNT - v_USEX_DCNT - v_AUSE_DCNT - v_DGNL_DCNT  --사용가능일수
        ,v_POCR_DCNT   --선사용발생일수
        ,v_PUSX_DCNT   --선사용사용일수
        ,v_POCR_DCNT - v_PUSX_DCNT   --선사용잔여일수
        ,v_ACUR_DCNT  --대체휴가 사용가능일수
        ,v_ORGC_DCNT  --원 발생일수
        ,CASE WHEN v_PCOP_JCDT IS NULL OR v_ANSD_DATE IS NULL OR ar_STDS_DATE < v_PCOP_JCDT THEN '' ELSE
            CASE WHEN TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'),24),'YYYYMMDD') > ar_STDS_DATE THEN TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'),24) - 1,'YYYYMMDD') ELSE
                TO_CHAR(TO_DATE(CASE WHEN SUBSTR(ar_STDS_DATE,5,4) < SUBSTR(v_ANSD_DATE, 5,4) THEN SUBSTR(ar_STDS_DATE,1,4) ELSE TO_CHAR(TO_NUMBER(SUBSTR(ar_STDS_DATE,1,4)) + 1) END ||  CASE WHEN SUBSTR(v_ANSD_DATE,5,4) = '0229' THEN '0301' ELSE SUBSTR(v_ANSD_DATE,5,4) END ,'YYYYMMDD')  - 1,'YYYYMMDD')
             END   --사용기한
         END
        ,v_BJIK_YSNO
        ,v_BJIK_YSNO2
        ,SYSDATE
    );

END IF;

 --IF  ar_EMPL_NUMB = '4514' THEN
 --IF  ar_EMPL_NUMB = '6595' THEN
 --IF  ar_EMPL_NUMB = '3985' THEN
-- IF  ar_EMPL_NUMB = '6180' THEN
-- IF  ar_EMPL_NUMB = '4446' THEN
-- IF  ar_EMPL_NUMB = '5445' THEN
-- IF  ar_EMPL_NUMB = '6073' THEN
-- IF  ar_EMPL_NUMB = '71031' THEN
-- IF  ar_EMPL_NUMB = '8063' THEN
--IF  ar_EMPL_NUMB = '3627' OR ar_EMPL_NUMB = '2895' THEN
IF  ar_EMPL_NUMB = '5309' THEN

     --MHS 2015.1.28 연차 기준자료 확인
     INSERT INTO  MHS_YEONCH_DATA (
        EMPL_NUMB ,
        STDS_DATE ,
        PCOP_JCDT ,  --공단입사일
        ANSD_DATE ,  --연차기준일
        CBGB_CODE ,  --보험/의료사업구분
        CONT_YEAR ,  --근무년차
        LAST_ANDT ,  --최근연차기준일
        NEXT_ANDT ,  --다음연차기준일
        SU1_DATE  ,  --선사용기준일(시작)
        SU2_DATE  ,  --선사용기준일(종료)
        US1_DATE  ,  --사용기준일(시작)
        US2_DATE  ,  --사용기준일(종료)
        OCUR_DCNT ,  --연차발생일수
        AOCU_DCNT ,  --추가발생일수
        PUSE_DCNT ,  --선사용차감일수
        USEX_DCNT ,  --사용일수
        INST_DATE ,
        BIGO
        ) VALUES (
            ar_EMPL_NUMB,
            ar_STDS_DATE,
            v_PCOP_JCDT,   --공단입사일
            v_ANSD_DATE,  --연차기준일
            v_CBGB_CODE,   --보험/의료사업구분
            v_CONT_YEAR,  --근무연차
            v_LAST_ANDT ,  --최근연차기준일
            v_NEXT_ANDT ,  --다음연차기준일
            TO_CHAR(ADD_MONTHS(TO_DATE(v_LAST_ANDT, 'YYYYMMDD'), -12), 'YYYYMMDD') , --선사용기준일(시작)
            TO_CHAR(TO_DATE(v_LAST_ANDT, 'YYYYMMDD') - 1, 'YYYYMMDD'),                --선사용기준일(종료)
            TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 12 * (v_CONT_YEAR-2)),'YYYYMMDD') , --사용기준일(시작)
            LEAST(ar_STDS_DATE, TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'), 12 * (v_CONT_YEAR-1))-1,'YYYYMMDD')) ,  --사용기준일(종료)
            v_OCUR_DCNT,   --연차발생일수
            v_AOCU_DCNT,   --추가발생일수
            v_PUSE_DCNT,   --선사용차감일수
            v_USEX_DCNT,   --사용일수
            SYSDATE ,
           'ar_USED_TYPE:: '||ar_USED_TYPE||' v_GUBUN:: '||v_GUBUN||'v_ORGC_DCNT:: '||v_ORGC_DCNT||'v_EXJA_CONT :: ' ||  v_WORK_DCNT || 'v_EXDV_DCNT_00 :: ' || v_EXDV_DCNT_00 || 'v_EXDV_DCNT :: ' || v_EXDV_DCNT ||  'v_WORK_RATE_00 :: ' || v_WORK_RATE_00 ||  'v_WORK_RATE :: ' || v_WORK_RATE ||  'v_EXCP_DATE :: ' || v_EXCP_DATE || 'v_BJIK_LAST_ANDT :: ' || v_BJIK_LAST_ANDT || 'v_EXCP_BEDE1 :: ' || v_EXCP_BEDE1 || 'v_EXCP_ENDD1 :: ' || v_EXCP_ENDD1
        );
END IF;

    <<RTNVALUE>>
    v_tab.extend;
    v_tab(v_tab.last) := RTYPE_SV_HOLIXM1(
         ar_EMPL_NUMB  --사원번호
        ,CASE WHEN v_PCOP_JCDT IS NULL OR v_ANSD_DATE IS NULL OR ar_STDS_DATE < v_PCOP_JCDT THEN '' ELSE CASE WHEN TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'),12),'YYYYMMDD') > ar_STDS_DATE THEN v_PCOP_JCDT ELSE v_ANSD_DATE END END   --연차기준일
        ,v_OCUR_DCNT   --연차발생일수
        ,v_AOCU_DCNT   --추가발생일수
        ,v_PUSE_DCNT   --선사용차감일수
        ,v_USEX_DCNT   --사용일수
        ,v_OCUR_DCNT + v_AOCU_DCNT - v_PUSE_DCNT - v_USEX_DCNT - v_AUSE_DCNT - v_DGNL_DCNT   --사용가능일수 --MHS 2015.3.19 사용(조정) 추가  인사부 박성민 과장 요청 ( 잔여일수에서 추가로 빼준다)
        ,v_POCR_DCNT   --선사용발생일수
        ,v_PUSX_DCNT   --선사용사용일수
        ,v_POCR_DCNT - v_PUSX_DCNT   --선사용잔여일수
        ,v_ACUR_DCNT  --대체휴가 사용가능일수
        ,v_ORGC_DCNT  --원 발생일수
        ,CASE WHEN v_PCOP_JCDT IS NULL OR v_ANSD_DATE IS NULL OR ar_STDS_DATE < v_PCOP_JCDT THEN '' ELSE
            CASE WHEN TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'),24),'YYYYMMDD') > ar_STDS_DATE THEN TO_CHAR(ADD_MONTHS(TO_DATE(v_ANSD_DATE,'YYYYMMDD'),24) - 1,'YYYYMMDD') ELSE
--                TO_CHAR(TO_DATE(CASE WHEN SUBSTR(ar_STDS_DATE,5,4) < SUBSTR(v_ANSD_DATE, 5,4) THEN SUBSTR(ar_STDS_DATE,1,4) ELSE TO_CHAR(TO_NUMBER(SUBSTR(ar_STDS_DATE,1,4)) + 1) END || SUBSTR(v_ANSD_DATE,5,4),'YYYYMMDD') - 1,'YYYYMMDD')
                TO_CHAR(TO_DATE(CASE WHEN SUBSTR(ar_STDS_DATE,5,4) < SUBSTR(v_ANSD_DATE, 5,4) THEN SUBSTR(ar_STDS_DATE,1,4) ELSE TO_CHAR(TO_NUMBER(SUBSTR(ar_STDS_DATE,1,4)) + 1) END ||  CASE WHEN SUBSTR(v_ANSD_DATE,5,4) = '0229' THEN '0301' ELSE SUBSTR(v_ANSD_DATE,5,4) END ,'YYYYMMDD')  - 1,'YYYYMMDD')
             END   --사용기한
         END
        ,v_AUSE_DCNT   --MHS 2015.3.19 사용(조정) 추가  인사부 박성민 과장 요청 ( 잔여일수에서 추가로 빼준다)
    );

    COMMIT;
    RETURN v_tab;

END SF_SV_HOLIXM;
```
