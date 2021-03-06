# ds_project_saveseaturtle

### 사용 데이터
- 전국 폐기물 발생 및 처리현황 출처 : 자원순환정보시스템
- 시도별 폐기물 배출량(총쓰레기, 종이류, 유리병류, 캔류) : 자원순환정보시스템
- 1인 가구수 출처: 통계청, 「인구총조사」
- 인구증가율 출처 : 통계청, 「인구총조사」, 「장래인구추계」
- 경제활동인구 출처 : 통계청, 「경제활동인구조사」
- 지역내총생산 출처 : 통계청, 「지역소득」
- 지역별 종사자수(제조업, 숙박및음식점업, 농림어업)  출처 : 통계청, 「전국사업체조사」
- 비알콜음료업 점포 수 : 통계청, 「전국사업체조사」
- 플라스틱 산업 부가가치 출처 : 산업통상자원부,「소재·부품산업동향조사」
- 취수시설 출처 : 환경부, 「수자원현황」
- 경제성장율 출처 : 한국은행, 「국민계정」
- 국제유가 출처 : 에너지경제연구원
- 서울시 배달업종별 이용 통화량 출처 : SKT Big Data Hub
- 과대포장금지법 출처: [국가법령정보센터](http://www.law.go.kr/법령/제품의포장재질ㆍ포장방법에관한기준등에관한규칙)

### 설명변수

1. 1인당 플라스틱 배출량
- 2002년부터 2017년에 전국 시도별 플라스틱 배출량 데이터 이용
- 절대적인 양은 자연스럽게 인구 수에 비례하기 때문에 인구 수에 대한 영향력을 제거하는 것이 적절하다고 판단
- 연도&지역별 인구수로 플라스틱 배출량을 나누어 1인당 플라스틱 배출량을 구함

### 독립변수

2. 서울시 배달업종별 이용 통화량
- 14~17년 서울특별시 배달업종별 이용 통화량 데이터
- 재활용 플라스틱(not 일반 플라스틱)과 배달량 추이 비슷 : 배달로 발생하는 플라스틱 쓰레기는 주로 재활용됨 

3. 고무-플라스틱 부가가치
- 플라스틱에 대한 수요를 나타낼 수 있을 것으로 예상됨

4. 

### 모델링
1. 기본적인 변수의 유의미성과 영향력을 보기 위해 다중회귀분석을 진행
2. RandomForest로 변수의 중요도를 파악하여 다중회귀분석에서 선정된 변수와 비교 

### ppt흐름
1. 주제 소개 
- 기사 크롤링 word cloud: 정확도순 연간 300개
- 플라스틱 사용 및 규제 등에 대한 여러 가지 사례들 제시
2. 독립변수 설명 
- 17개의 독립변수
- 이후 Variable selection으로 선정

### Xaringan
ppt용 css 파일 & 예시 html 추가함. 로컬 워킹 디렉토리에 css 파일 있어야 예시처럼 돌아감!   
ppt에 사용 가능한 option은 dsXaringan.css 와 Middlebury template(패키지 내장)에 있는 옵션 전부.   
전반적 템플릿 기반은 Middlebury, dsXaringan file은 몇몇 옵션만 추가함
