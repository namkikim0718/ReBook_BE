# ReBook_BE
<img width="811" alt="스크린샷 2024-09-11 23 33 14" src="https://github.com/user-attachments/assets/d04690c8-fafc-4a50-b182-3fee3f6d2ae7">


</br>

</br>

### 📽️ 시연 영상
https://youtu.be/msiGwnS4l-Q?si=rC5baI9Mo6tcP4zG 

</br>

### 🔖 프로젝트 개요
  - 주제 : 클라우드 기반 중고 전공 서적 거래를 위한 e-커머스 플랫폼
  - 대상 : 대학 재학생 및 졸업생

</br>

### 🎯 목표
  - 대학생과 졸업생에게 편리한 전공서적 중고거래 환경 제공
  - 사용자 간 직접 거래와 플랫폼의 안전 장치를 결합한 신뢰성 높은 전공서적 중고거래 플랫폼 구축

</br>

### 📚 기술 스택

|파트|프레임워크 & 라이브러리|
|---|---|
|Frontend|`React`, `Vite`|
|Backend|`Java`, `Spring Boot`, `Spring Security`, `Redis`, `Querydsl`, `STOMP`, `Netflix Eureka`|
|AI/ML|`Fast API`, `PyTorch`, `Pandas`, `KcELECTRA`|
|Infra|`Docker`, `AWS`, `Ansible`, `Terraform`, `Prometheus`, `Grafana`, `Loki`, `Jenkins`|
|Collaborative|`Notion`, `Figma`, `Github`|


</br>

### 🗂️ 개발 레포 및 문서
> 아래 링크를 통해 저희 서비스의 개발 레포지토리 및 문서를 확인하실 수 있습니다.

- [Frontend Repository](https://github.com/5-ReBook/ReBook_FE)
- [BackEnd Repository](https://github.com/5-ReBook/ReBook_BE)
- [AI Repository](https://github.com/5-ReBook/ReBook_AI)


</br>

### 🌏 서버 아키텍쳐
<img width="993" alt="스크린샷 2024-09-11 23 06 07" src="https://github.com/user-attachments/assets/ed1a8498-2a35-4240-a50d-924093bd82c0">

</br>

</br>

### 🔗 주요 기능

  #### 1️⃣ 회원 및 인증 관리 시스템
  - 카카오 소셜 로그인과 이메일 인증 로그인을 통해 회원 가입 및 로그인을 할 수 있습니다.
  - 마이페이지에서 회원 정보를 관리할 수 있습니다.

  #### 2️⃣ 상품 관리
  - 상품을 이미지와 함께 등록/수정/삭제 할 수 있습니다.
  - 상품을 검색 필터와 함께 조회할 수 있습니다.

  #### 3️⃣ 채팅
  - 사용자 간의 1:1 채팅을 통해 판매자와 구매자가 대화를 나눌 수 있습니다.
  - AI 모듈을 통해 채팅 내용이 사기 의심 메시지인지 판별해 경고 문구를 제공합니다.

</br>

</br>

### 🚀 트러블 슈팅

  #### 1️⃣ **상품 목록을 조회할 경우 검색 필터가 많아질수록 쿼리 파라미터가 많아져 코드 가독성 저하**
  - DTO를 만들어 @RequestParam 대신 @ModelAttribute를 통해 값들을 바인딩
  - Querydsl을 활용한 동적쿼리 생성
  #### 2️⃣ **페이지네이션 성능 개선**
  - 기존 offset 활용 방식으로 진행시 데이터가 많아질수록 응답속도가 점점 느려지는 것을 발견
  - no-offset 방식 페이지네이션을 적용해보려 했으나, 정렬 조건 때문에 적합하지 않다고 판단
  - 커서 기반의 페이지네이션을 적용해 10만건의 데이터 기준 약 <ins>98.68%</ins> 의 성능 개선 (**151ms → 2ms**)
  #### 3️⃣ **파일 업로드시 100MB보다 크기가 크면 413 에러가 발생**
  - Nginx.conf에서 client_max_body_size를 높게 설정
  - application.yml 에서 multipart max_file_size와 max_request_size를 수정해 해결

