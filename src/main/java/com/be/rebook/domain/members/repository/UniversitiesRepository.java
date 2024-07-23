package com.be.rebook.domain.members.repository;

import com.be.rebook.domain.members.entity.Universities;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UniversitiesRepository extends JpaRepository<Universities, Long> {
    //전공 입력할때 현재 입력한 문자열이 존재하는 모든 전공들 다 가져오는 쿼리 조건에 맞는거 전부 TODO

    //컬럼 딱 두개있어서 따로 인터페이스 선언해서 id값만 학교만 이런건 안함
    //학교 이름으로 id값 가져오기 (회원정보 수정할때 필요)
    Universities findByUniversity(String university);

    //id값으로 학교 이름가져오기 (데이터 조회시에 필요)
    Optional<Universities> findByUnvId(Long id);
}
