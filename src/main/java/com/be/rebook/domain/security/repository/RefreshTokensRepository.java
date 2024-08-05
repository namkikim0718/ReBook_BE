package com.be.rebook.domain.security.repository;

import com.be.rebook.domain.security.entity.RefreshTokens;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public interface RefreshTokensRepository extends JpaRepository<RefreshTokens, Long> {
    Boolean existsByRefresh(String refresh);

    @Transactional
    void deleteByRefresh(String refresh);

    //매일 자정에 하루 지난 리프레쉬 삭제함
    @Transactional
    @Modifying
    @Query(value = "DELETE FROM RefreshEntity r WHERE r.expiration < :cutoff", nativeQuery = true)
    void deleteOldRefresh(String cutoff);


    List<RefreshTokens> findByUsername(String username);
}