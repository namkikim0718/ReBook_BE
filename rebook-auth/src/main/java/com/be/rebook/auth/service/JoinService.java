package com.be.rebook.auth.service;

import com.be.rebook.auth.dto.VerifyDTO;
import com.be.rebook.auth.entity.Members;
import com.be.rebook.auth.dto.JoinDTO;
import com.be.rebook.auth.repository.MembersRepository;
import com.be.rebook.auth.utility.InputVerifier;
import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;

import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.security.SecureRandom;

@Service
public class JoinService {

    private final MembersRepository membersRepository;
    private final BCryptPasswordEncoder bCryptPasswordEncoder;

    //todo : RedisTemplate 사용하는 인터페이스 만들기
    private final RedisManagerImpl redisManager;

    private final EmailService emailService;

    public JoinService(MembersRepository membersRepository,
                       BCryptPasswordEncoder bCryptPasswordEncoder,
                       RedisManagerImpl redisManager,
                       EmailService emailService){
        this.membersRepository = membersRepository;
        this.bCryptPasswordEncoder = bCryptPasswordEncoder;
        this.redisManager = redisManager;
        this.emailService = emailService;
    }

    public Members joinProcess(JoinDTO joinDTO){
        String username = joinDTO.getUsername();

        if(Boolean.FALSE.equals(InputVerifier.checkUsernameCharacters(username))){
            //BAD_INPUT
            throw new BaseException(ErrorCode.BAD_INPUT);
        }

        String password = joinDTO.getPassword();
        if(Boolean.FALSE.equals(InputVerifier.checkPasswordCharacters(password))){
            //BAD_INPUT
            throw new BaseException(ErrorCode.BAD_INPUT);
        }

        Boolean isExist = membersRepository.existsByUsername(username);

        if(Boolean.TRUE.equals(isExist)){
            //EXISTING_USER_INFO
            throw new BaseException(ErrorCode.EXISTING_USER_INFO);
        }

        Members data = Members.builder()
                .username(username)
                .password(bCryptPasswordEncoder.encode(password+username))
                .role("ROLE_USER")
                .build();

        membersRepository.save(data);
        return data;
    }

    private String generateVerificationCode() {
        SecureRandom random = new SecureRandom();
        int code = random.nextInt(900000) + 100000; // 100000 ~ 999999 사이의 숫자
        return String.valueOf(code);
    }

    public Members sendVerification(String username) {

        Boolean isExist = membersRepository.existsByUsername(username);

        if (Boolean.TRUE.equals(isExist)) {
            // EXISTING_USER_INFO
            throw new BaseException(ErrorCode.EXISTING_USER_INFO);
        }

        // 6자리 랜덤 인증번호 생성
        String verificationCode = generateVerificationCode();


        // Redis에 인증번호 저장 (키: username, 값: verificationCode, 유효시간: 3분)
        redisManager.setValuesWithDuration(username,verificationCode);

        // 이메일 전송
        emailService.sendVerificationEmail(username, verificationCode);

        return new Members();
    }

    public Members verifyCode(VerifyDTO verifyDTO) {
        String username = verifyDTO.getUsername();
        String password = verifyDTO.getPassword();
        String code = verifyDTO.getCode();
        String storedVerificationCode = null;

        // Redis에서 username을 키로 가지는 저장된 인증번호 불러오기
        storedVerificationCode = redisManager.getValue(username);

        if (!storedVerificationCode.equals(code)) {
            // BAD_REQUEST
            throw new BaseException(ErrorCode.MAIL_AUTH_CODE_INCORRECT);
        }

        // 인증번호가 일치하면 회원 정보 생성 및 저장
        Members data = Members.builder()
                .username(username)
                .password(bCryptPasswordEncoder.encode(password + username))
                .role("ROLE_USER")
                .build();

        membersRepository.save(data);
        return data;
    }
}
