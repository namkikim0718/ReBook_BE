package com.be.rebook.members.repository;

import com.be.rebook.members.entity.Members;
import org.springframework.data.jpa.repository.JpaRepository;

public interface MembersRepository extends JpaRepository<Members, Integer> {

    Boolean existsByUsername(String username);

    //유저를 조회하는 메서드
    Members findByUsername(String username);
}